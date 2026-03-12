/*!
 * KMZ EMR Standard - BOQ Excel Generation Worker
 * Copyright (c) 2026 Muhammad Ikhsanudin
 * * This file is part of KMZ EMR Standard.
 * This software is licensed under the PolyForm Strict License 1.0.0.
 * * STRICT PROHIBITION: You may use this software for your own purposes, 
 * but you MAY NOT modify, copy, or distribute this software or any 
 * modified version of it without prior written consent from the Copyright Holder.
 * * A copy of the license is located in the LICENSE file in the root directory, 
 * or can be found at: https://polyformproject.org/licenses/strict/1.0.0/
 */
 
// js/boq.worker.js
importScripts('https://cdnjs.cloudflare.com/ajax/libs/exceljs/4.3.0/exceljs.min.js');

const MAP_CLUSTER = {
    cable: {
        'A': { '24C': 2, '36C': 6, '48C': 10 },
        'B': { '24C': 3, '36C': 7, '48C': 11 },
        'C': { '24C': 4, '36C': 8, '48C': 12 },
        'D': { '24C': 5, '36C': 9, '48C': 13 }
    },
    sling: 15,
    tektok: 81,
    fdt:   { '48C': 30, '72C': 31, '96C': 32, '144C': 33 },
    fat:   { 'A': 36, 'B': 37, 'C': 38, 'D': 39 },
    pole:  {
        'NEW POLE 7-4': 54, 'NEW POLE 7-3': 55, 'NEW POLE 7-2.5': 56, 'NEW POLE 9-4': 58,
        'EXISTING POLE PARTNER': 59, 'EXISTING POLE EMR': 61
    }
};

const MAP_SUBFEEDER = {
    cable: { '24C': 2, '36C': 3, '48C': 4, '72C': 5, '96C': 6, '144C': 7, '288C': 8 },
    jc:    { '24C': 23, '36C': 24, '48C': 25, '72C': 26, '96C': 27, '144C': 28, '288C': 29 },
    pole:  {
        'NEW POLE 7-4': 49,
        'NEW POLE 7-5': 52,
        'NEW POLE 9-4': 53,
        'NEW POLE 9-5': 54,
        'EXISTING POLE PARTNER': 55,
        'EXISTING POLE': 59
    }
};

self.onmessage = async function(e) {
    try {
        const { boqData, templateBuffer, mode = 'cluster' } = e.data;
        const wb = new ExcelJS.Workbook();
        await wb.xlsx.load(templateBuffer);
        
        const ws = wb.worksheets[3]; // Asumsi sheet index 3 (Sheet ke-4)

        if (mode === 'subfeeder') {
            // ================================
            // WRITE EXCEL: SUBFEEDER
            // ================================
            Object.keys(boqData).forEach(fdtStr => {
                const fdtNum = parseInt(fdtStr);
                // Di mapping lu, FDT 1 ada di kolom P (index Excel = 16).
                // Berarti perhitungannya: 15 + FDT 1 = 16 (Kolom P). FDT 2 = 17 (Kolom Q).
                const colIdx = 15 + fdtNum; 
                const data = boqData[fdtStr];

                // 1. Suntik Kabel
                Object.keys(data.cables).forEach(cap => {
                    const row = MAP_SUBFEEDER.cable[cap];
                    if (row) {
                        const curVal = ws.getCell(row, colIdx).value || 0;
                        ws.getCell(row, colIdx).value = curVal + data.cables[cap].routeLen;
                    }
                });

                // 2. Suntik Joint Closure (JC)
                Object.keys(data.jc).forEach(cap => {
                    const row = MAP_SUBFEEDER.jc[cap];
                    if (row) {
                        const curVal = ws.getCell(row, colIdx).value || 0;
                        ws.getCell(row, colIdx).value = curVal + data.jc[cap];
                    }
                });

                // 3. Suntik Pole
                Object.keys(data.poles).forEach(poleName => {
                    let targetRow = null;
                    if (poleName.includes('NEW POLE 7-4')) targetRow = MAP_SUBFEEDER.pole['NEW POLE 7-4'];
                    else if (poleName.includes('NEW POLE 7-5')) targetRow = MAP_SUBFEEDER.pole['NEW POLE 7-5'];
                    else if (poleName.includes('NEW POLE 9-4')) targetRow = MAP_SUBFEEDER.pole['NEW POLE 9-4'];
                    else if (poleName.includes('NEW POLE 9-5')) targetRow = MAP_SUBFEEDER.pole['NEW POLE 9-5'];
                    else if (poleName.includes('EXISTING')) targetRow = MAP_SUBFEEDER.pole['EXISTING POLE']; // Semua existing ke P59

                    if (targetRow) {
                        const curVal = ws.getCell(targetRow, colIdx).value || 0;
                        ws.getCell(targetRow, colIdx).value = curVal + data.poles[poleName];
                    }
                });
            });

        } else {
            // ================================
            // WRITE EXCEL: CLUSTER (ORIGINAL)
            // ================================
            Object.keys(boqData).forEach(fdtStr => {
                const fdtNum = parseInt(fdtStr);
                const colIdx = 3 + fdtNum; // FDT 1 = Kolom D (4)
                const data = boqData[fdtStr];

                if (data.fdtCap && MAP_CLUSTER.fdt[data.fdtCap]) {
                    const row = MAP_CLUSTER.fdt[data.fdtCap];
                    ws.getCell(row, colIdx).value = (ws.getCell(row, colIdx).value || 0) + 1;
                }

                let totalFdtSling = 0;
                let totalFdtTektok = 0; 

                Object.keys(data.lines).forEach(lineChar => {
                    const lineInfo = data.lines[lineChar];
                    if (lineInfo.cableCap && lineInfo.routeLen > 0) {
                        const row = MAP_CLUSTER.cable[lineChar]?.[lineInfo.cableCap];
                        if (row) ws.getCell(row, colIdx).value = lineInfo.routeLen;
                    }
                    if (lineInfo.fatCount > 0) {
                        const row = MAP_CLUSTER.fat[lineChar];
                        if (row) ws.getCell(row, colIdx).value = lineInfo.fatCount;
                    }
                    if (lineInfo.slingLen > 0) totalFdtSling += lineInfo.slingLen;
                    if (lineInfo.tektokLen > 0) totalFdtTektok += lineInfo.tektokLen; 
                });

                if (totalFdtSling > 0) ws.getCell(MAP_CLUSTER.sling, colIdx).value = totalFdtSling;
                if (totalFdtTektok > 0) ws.getCell(MAP_CLUSTER.tektok, colIdx).value = totalFdtTektok; 

                Object.keys(data.poles).forEach(poleName => {
                    let mappedRow = null;
                    if (poleName.includes('NEW POLE 7-4')) mappedRow = MAP_CLUSTER.pole['NEW POLE 7-4'];
                    else if (poleName.includes('NEW POLE 7-3')) mappedRow = MAP_CLUSTER.pole['NEW POLE 7-3'];
                    else if (poleName.includes('NEW POLE 7-2.5')) mappedRow = MAP_CLUSTER.pole['NEW POLE 7-2.5'];
                    else if (poleName.includes('NEW POLE 9-4')) mappedRow = MAP_CLUSTER.pole['NEW POLE 9-4'];
                    else if (poleName.includes('EXISTING POLE PARTNER')) mappedRow = MAP_CLUSTER.pole['EXISTING POLE PARTNER'];
                    else if (poleName.includes('EXISTING POLE EMR')) mappedRow = MAP_CLUSTER.pole['EXISTING POLE EMR'];

                    if (mappedRow) {
                        const curVal = ws.getCell(mappedRow, colIdx).value || 0;
                        ws.getCell(mappedRow, colIdx).value = curVal + data.poles[poleName];
                    }
                });
            }); 
        }

        const outBuffer = await wb.xlsx.writeBuffer();
        self.postMessage({ status: 'success', buffer: outBuffer });
        
    } catch(err) {
        self.postMessage({ status: 'error', error: err.message });
    }
};