/*!
 * KMZ EMR Standard - BOQ Auditor
 * Copyright (c) 2026 Muhammad Ikhsanudin
 * * This file is part of KMZ EMR Standard.
 * This software is licensed under the PolyForm Strict License 1.0.0.
 * * STRICT PROHIBITION: You may use this software for your own purposes, 
 * but you MAY NOT modify, copy, or distribute this software or any 
 * modified version of it without prior written consent from the Copyright Holder.
 * * A copy of the license is located in the LICENSE file in the root directory, 
 * or can be found at: https://polyformproject.org/licenses/strict/1.0.0/
 */

import { 
    restructureKML, 
    splitSlingWires, 
    autoCorrectHierarchy, 
    injectDescriptionsAndCalc 
} from '../kmz-emr-standard/js/logic-features.js';
import { extractBOQData } from '../kmz-emr-standard/js/generateBOQ.js';

// --- MAPPING EXCEL (Kopi Paste dari boq.worker.js lu) ---
const MAP_CLUSTER = {
    cable: { 'A': { '24C': 2, '36C': 6, '48C': 10 }, 'B': { '24C': 3, '36C': 7, '48C': 11 }, 'C': { '24C': 4, '36C': 8, '48C': 12 }, 'D': { '24C': 5, '36C': 9, '48C': 13 } },
    sling: 15, tektok: 81, 
    fdt: { '48C': 30, '72C': 31, '96C': 32, '144C': 33 }, 
    fat: { 'A': 36, 'B': 37, 'C': 38, 'D': 39 },
    pole: { 'NEW POLE 7-4': 54, 'NEW POLE 7-3': 55, 'NEW POLE 7-2.5': 56, 'NEW POLE 9-4': 58, 'EXISTING POLE PARTNER': 59, 'EXISTING POLE EMR': 61 }
};

const cleanFileName = (filename) => {
    let name = filename.toLowerCase();
    name = name.replace(/\.(kmz|kml|xlsx|xls)$/, '');
    name = name.replace(/^[^-_]+[-_]\s*/, '');
    const stopWords = ['boq', 'vendor', 'desain', 'draft', 'final', 'rev', 'copy', 'revisi'];
    stopWords.forEach(word => { name = name.replace(new RegExp(`\\b${word}\\b`, 'g'), ''); });
    name = name.replace(/[^a-z0-9]/g, '');
    return name;
};

// --- HELPER 1: KOORDINAT EXCEL SCANNER (Sesuai Ide Lu!) ---
const readVendorExcelByMap = async (file) => {
    const data = await file.arrayBuffer();
    const workbook = XLSX.read(data, { type: 'array' });
    const sheetName = workbook.SheetNames[3]; // Sheet ke-4 (Index 3)
    const sheet = workbook.Sheets[sheetName];

    const vendorTotals = { lines: {}, sling: 0, poles: {} };
    ['A', 'B', 'C', 'D'].forEach(l => vendorTotals.lines[l] = {});

    // Fungsi sakti buat nembak sel langsung pakai koordinat baris dari MAP_CLUSTER
    const getVal = (row, colIndex) => {
        if (!row) return 0;
        const cell = sheet[XLSX.utils.encode_cell({ r: row - 1, c: colIndex })]; // r dikurangi 1 krn SheetJS 0-based
        return cell ? (parseFloat(cell.v) || 0) : 0;
    };

    const TOTAL_COL = 2; // Kolom C (Index 2) adalah Kolom "Qty" Total Cluster

    // 1. Ekstrak Kabel persis dari Baris yang ditunjuk MAP_CLUSTER
    ['A', 'B', 'C', 'D'].forEach(line => {
        ['24C', '36C', '48C'].forEach(cap => {
            vendorTotals.lines[line][cap] = getVal(MAP_CLUSTER.cable[line][cap], TOTAL_COL);
        });
    });

    // 2. Ekstrak Sling Wire (Tembak Baris 15)
    vendorTotals.sling = getVal(MAP_CLUSTER.sling, TOTAL_COL);

    // 3. Ekstrak Tiang (Tembak Baris 54, 55, dst)
    Object.keys(MAP_CLUSTER.pole).forEach(pType => {
        vendorTotals.poles[pType] = getVal(MAP_CLUSTER.pole[pType], TOTAL_COL);
    });

    return vendorTotals;
};

// --- HELPER 2: BACA & AGGREGATE KML SYSTEM ---
const processKMZAgregate = async (file) => {
    let xmlDoc;
    if (file.name.toLowerCase().endsWith('.kmz')) {
        const zip = new JSZip();
        const contents = await zip.loadAsync(file);
        const kmlKey = Object.keys(contents.files).find(n => n.endsWith('.kml'));
        const kmlString = await contents.files[kmlKey].async('string');
        xmlDoc = new DOMParser().parseFromString(kmlString, 'text/xml');
    } else {
        xmlDoc = new DOMParser().parseFromString(await file.text(), 'text/xml');
    }

    // 1. JALANKAN PIPELINE KML SECARA FULL BIAR MATANG!
    const { rootName } = restructureKML(xmlDoc);
    splitSlingWires(xmlDoc); // Potong sling wire
    autoCorrectHierarchy(xmlDoc, 'cluster', true); // Pindahin ke LINE masing-masing
    injectDescriptionsAndCalc(xmlDoc, 'cluster', true); // Kalkulasi panjangnya

    // 2. Baru Ekstrak Datanya
    const rawBoq = extractBOQData(xmlDoc, rootName, 'cluster');

    const systemTotals = { lines: {}, sling: 0, poles: {} };
    ['A', 'B', 'C', 'D'].forEach(l => systemTotals.lines[l] = {});

    // Lebur data Per-FDT dari KML jadi 1 Total besar
    Object.values(rawBoq).forEach(fdt => {
        Object.keys(fdt.lines).forEach(l => {
            const info = fdt.lines[l];
            if (info.cableCap && info.routeLen > 0 && systemTotals.lines[l]) {
                systemTotals.lines[l][info.cableCap] = (systemTotals.lines[l][info.cableCap] || 0) + info.routeLen;
            }
            // Ambil Sling Wire dari tiap Line
            if (info.slingLen > 0) systemTotals.sling += info.slingLen;
        });

        // Fallback: Siapa tau kalau mode subfeeder dia nyimpen di luar line
        if (fdt.sling > 0) systemTotals.sling += fdt.sling;

        const poleObj = fdt.poles || fdt.pole || {};
        Object.entries(poleObj).forEach(([pType, count]) => {
            if (count > 0) {
                let mappedKey = null;
                if (pType.includes('NEW POLE 7-4')) mappedKey = 'NEW POLE 7-4';
                else if (pType.includes('NEW POLE 7-3')) mappedKey = 'NEW POLE 7-3';
                else if (pType.includes('NEW POLE 7-2.5')) mappedKey = 'NEW POLE 7-2.5';
                else if (pType.includes('NEW POLE 9-4')) mappedKey = 'NEW POLE 9-4';
                else if (pType.includes('EXISTING POLE PARTNER')) mappedKey = 'EXISTING POLE PARTNER';
                else if (pType.includes('EXISTING POLE EMR')) mappedKey = 'EXISTING POLE EMR';
                
                if (mappedKey) systemTotals.poles[mappedKey] = (systemTotals.poles[mappedKey] || 0) + count;
            }
        });
    });

    return systemTotals;
};

document.addEventListener('DOMContentLoaded', () => {
    const fileInput = document.getElementById('fileInput');
    const dropZone = document.getElementById('dropZone');
    const previewContainer = document.getElementById('previewContainer');
    const tbody = document.querySelector('#pairingTable tbody');
    const runBtn = document.getElementById('runAuditBtn');
    
    const resetAuditBtn = document.getElementById('resetAuditBtn');
    const downloadPdfBtn = document.getElementById('downloadPdfBtn');
    const actionButtons = document.getElementById('actionButtons');

    let pairedFiles = []; 
    const globalSiteMap = new Map(); 

    const processFiles = (files) => {
        // 1. Munculin UI Loading & Spinner instan tanpa babibu
        document.getElementById('dropZoneContent').style.display = 'none';
        document.getElementById('dropZoneLoading').style.display = 'flex';
        
        // 2. Gunakan Double requestAnimationFrame
        // Ini trik pro biar browser nge-render spinner-nya dulu ke layar, 
        // BARU dia ngejalanin tugas berat (ngejodohin nama & nyetak tabel).
        requestAnimationFrame(() => {
            requestAnimationFrame(() => {
                const fileArray = Array.from(files);
                const kmzFiles = fileArray.filter(f => f.name.toLowerCase().match(/\.(kmz|kml)$/));
                const excelFiles = fileArray.filter(f => f.name.toLowerCase().match(/\.(xlsx|xls)$/));

                kmzFiles.forEach(f => {
                    const coreName = cleanFileName(f.name);
                    if (!globalSiteMap.has(coreName)) globalSiteMap.set(coreName, { kmz: null, excel: null, originalName: f.name.replace(/\.(kmz|kml)$/i, '') });
                    globalSiteMap.get(coreName).kmz = f;
                });

                excelFiles.forEach(f => {
                    const coreName = cleanFileName(f.name);
                    if (!globalSiteMap.has(coreName)) globalSiteMap.set(coreName, { kmz: null, excel: null, originalName: f.name.replace(/\.(xlsx|xls)$/i, '') });
                    globalSiteMap.get(coreName).excel = f;
                });

                // 3. Pakai DocumentFragment buat nyetak baris HTML
                // Bikin tabelnya di "memory" dulu, jgn langsung ke layar biar gak ngelag
                const fragment = document.createDocumentFragment();
                pairedFiles = [];
                let readyToRun = false;

                globalSiteMap.forEach((pair, coreName) => {
                    pairedFiles.push(pair);
                    const isComplete = pair.kmz && pair.excel;
                    if (isComplete) readyToRun = true;

                    const tr = document.createElement('tr');
                    tr.innerHTML = `
                        <td style="font-weight: bold;">${pair.originalName}</td>
                        <td>${pair.kmz ? `✅ ${pair.kmz.name}` : `<span style="color:#8b949e">Missing KMZ</span>`}</td>
                        <td>${pair.excel ? `✅ ${pair.excel.name}` : `<span style="color:#8b949e">Missing Excel</span>`}</td>
                        <td><span class="status-badge ${isComplete ? 'bg-green' : 'bg-red'}">${isComplete ? 'Ready to Audit' : 'Missing Pair'}</span></td>
                    `;
                    fragment.appendChild(tr); // Masukin ke memory
                });

                // Tembakin isi memory ke layar dalam 1x eksekusi (Super Cepat!)
                tbody.innerHTML = '';
                tbody.appendChild(fragment);

                if (globalSiteMap.size > 0) previewContainer.style.display = 'block';
                runBtn.disabled = !readyToRun;

                // 4. Balikin UI Drag & Drop ke wujud asli
                document.getElementById('dropZoneLoading').style.display = 'none';
                document.getElementById('dropZoneContent').style.display = 'block';
            });
        });
    };

    fileInput.addEventListener('change', (e) => { processFiles(e.target.files); fileInput.value = ''; });
    dropZone.addEventListener('dragover', (e) => { e.preventDefault(); dropZone.classList.add('dragover'); });
    dropZone.addEventListener('dragleave', () => { dropZone.classList.remove('dragover'); });
    dropZone.addEventListener('drop', (e) => {
        e.preventDefault(); dropZone.classList.remove('dragover');
        if (e.dataTransfer.files.length > 0) processFiles(e.dataTransfer.files);
    });

 // --- TAHAP 2: EKSEKUSI AUDIT (SUPER FAST PARALLEL) ---
    runBtn.addEventListener('click', async () => {
        runBtn.innerHTML = '<span class="loading-spinner"></span> Auditing...';
        runBtn.disabled = true;

        // 1. Ambil file yang valid aja
        const filesToAudit = pairedFiles.filter(p => p.kmz && p.excel);

        // 2. JALANKAN SECARA PARALEL (Semua site diproses serentak)
        const auditPromises = filesToAudit.map(async (pair) => {
            try {
                // 3. JALANKAN KMZ & EXCEL BARENGAN! (Hemat waktu 50%)
                const [sysTotals, venTotals] = await Promise.all([
                    processKMZAgregate(pair.kmz),
                    readVendorExcelByMap(pair.excel)
                ]);

                let isClear = true;
                let discrepancies = [];

                // A. Bandingkan Kabel
                ['A','B','C','D'].forEach(line => {
                    ['24C', '36C', '48C'].forEach(cap => {
                        const sysCable = sysTotals.lines[line][cap] || 0;
                        const venCable = venTotals.lines[line][cap] || 0;
                        const diff = venCable - sysCable;

                        if (diff !== 0) {
                            isClear = false;
                            const diffText = diff > 0 ? `<span style="color:#f85149">OVERCLAIM (+${diff}m)</span>` : `<span style="color:#d2a8ff">UNDERCLAIM (${diff}m)</span>`;
                            discrepancies.push(`Line ${line} - ${cap} | KML: ${sysCable}m | Vendor: ${venCable}m ➔ ${diffText}`);
                        }
                    });
                });

                // B. Bandingkan Sling Wire
                const slingDiff = venTotals.sling - sysTotals.sling;
                if (slingDiff !== 0) {
                    isClear = false;
                    const diffText = slingDiff > 0 ? `<span style="color:#f85149">OVERCLAIM (+${slingDiff}m)</span>` : `<span style="color:#d2a8ff">UNDERCLAIM (${slingDiff}m)</span>`;
                    discrepancies.push(`Sling Wire | KML: ${sysTotals.sling}m | Vendor: ${venTotals.sling}m ➔ ${diffText}`);
                }

                // C. Bandingkan Tiang
                Object.keys(MAP_CLUSTER.pole).forEach(pType => {
                    const sysPole = sysTotals.poles[pType] || 0;
                    const venPole = venTotals.poles[pType] || 0;
                    const pDiff = venPole - sysPole;
                    if (pDiff !== 0) {
                        isClear = false;
                        const diffText = pDiff > 0 ? `<span style="color:#f85149">OVERCLAIM (+${pDiff} unit)</span>` : `<span style="color:#d2a8ff">UNDERCLAIM (${pDiff} unit)</span>`;
                        discrepancies.push(`Tiang (${pType}) | KML: ${sysPole} | Vendor: ${venPole} ➔ ${diffText}`);
                    }
                });

                return { site: pair.originalName, isClear, discrepancies };

            } catch (err) {
                console.error("Error processing " + pair.originalName, err);
                return { site: pair.originalName, isClear: false, discrepancies: [`⚠️ GAGAL BACA DATA: ${err.message}`] };
            }
        });

        // 4. TUNGGU SEMUA PROSES PARALEL SELESAI
        const auditResults = await Promise.all(auditPromises);

        // 5. Render ke Tabel dengan kecepatan cahaya
        const fragment = document.createDocumentFragment();
        auditResults.forEach(res => {
            const tr = document.createElement('tr');
            let detailsHtml = '';
            if (res.isClear) {
                detailsHtml = `<span style="color:#3fb950; font-weight:bold;">✅ All matched. Total Cluster Sesuai Desain!</span>`;
            } else {
                detailsHtml = `<ul style="margin:0; padding-left:15px; color:#c9d1d9; font-family:monospace; font-size:12px;">
                    ${res.discrepancies.map(d => `<li style="margin-bottom:4px;">${d}</li>`).join('')}
                </ul>`;
            }

            tr.innerHTML = `
                <td style="font-weight: bold; vertical-align: top;">${res.site}</td>
                <td colspan="2">${detailsHtml}</td>
                <td style="vertical-align: top;">
                    <span class="status-badge ${res.isClear ? 'bg-green' : 'bg-red'}">
                        ${res.isClear ? 'PASSED' : 'DISCREPANCY DETECTED'}
                    </span>
                </td>
            `;
            fragment.appendChild(tr);
        });

        tbody.innerHTML = '';
        tbody.appendChild(fragment);

        const thead = document.querySelector('#pairingTable thead tr');
        thead.innerHTML = `<th>Site / Core Name</th><th colspan="2">Audit Details (Total Cluster Summary)</th><th>Status</th>`;

        runBtn.innerHTML = '✅ Audit Complete';
        runBtn.classList.remove('btn-primary');
        runBtn.classList.add('btn-secondary');
        runBtn.style.cursor = 'default';
        downloadPdfBtn.style.display = 'inline-flex';
    });

    if (resetAuditBtn) {
        resetAuditBtn.addEventListener('click', () => {
            globalSiteMap.clear();
            pairedFiles = [];
            tbody.innerHTML = '';
            const thead = document.querySelector('#pairingTable thead tr');
            thead.innerHTML = `<th>Site / Core Name</th><th>KMZ Design File</th><th>Excel BOQ File</th><th>Status</th>`;
            
            runBtn.disabled = true;
            runBtn.classList.remove('btn-secondary');
            runBtn.classList.add('btn-primary');
            runBtn.innerHTML = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polygon points="5 3 19 12 5 21 5 3"></polygon></svg> Run Batch Audit';
            runBtn.style.cursor = 'pointer';
            
            previewContainer.style.display = 'none';
            downloadPdfBtn.style.display = 'none'; 
            fileInput.value = ''; 
        });
    }

    if (downloadPdfBtn) {
        downloadPdfBtn.addEventListener('click', () => {
            const originalContent = downloadPdfBtn.innerHTML;
            downloadPdfBtn.innerHTML = '⏳ Generating...';
            downloadPdfBtn.disabled = true;
            actionButtons.style.display = 'none';

            const opt = {
                margin:       0.3,
                filename:     'Batch_Audit_Report.pdf',
                image:        { type: 'jpeg', quality: 0.98 },
                html2canvas:  { scale: 2, backgroundColor: '#0d1117' }, 
                jsPDF:        { unit: 'in', format: 'a4', orientation: 'landscape' }
            };

            html2pdf().set(opt).from(previewContainer).save().then(() => {
                actionButtons.style.display = 'flex';
                downloadPdfBtn.innerHTML = originalContent;
                downloadPdfBtn.disabled = false;
            });
        });
    }
});