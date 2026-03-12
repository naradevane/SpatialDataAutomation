📖 Detail Penggunaan Script

**1. Spatial Manager Auto Injector (SPMAuto.lsp)**

Command: SPMAUTO



Fungsi: Menginjeksi nama secara berurutan (contoh: KLP.048.A-01, KLP.048.A-02) langsung ke dalam Extended Entity Data (EED) polygon sebagai tabel KML\_DATA.



Cara Pakai: Ketik command, masukkan Prefix dan nomor urut awal. Pilih polygon satu per satu dengan klik langsung atau blok menggunakan kotak seleksi (ijo/biru). Tekan spasi/enter untuk selesai.



**2. Rename Queue v2 (Rename Queue (RQ)v2.lsp)**

Command: RQ



Fungsi: Me-rename banyak TEXT, MTEXT, atau Block Attribute (POLE\_ID) sekaligus secara massal dan berurutan.



Cara Pakai: Seleksi objek, lalu LISP akan mengurutkan otomatis berdasarkan posisi alfabetis (A-Z atau Z-A). Mendukung format penamaan kompleks: Prefix + Nomor urut ber-padding (001, 002) + Suffix.



**3. Auto Close \& Shrink Polyline (SHRINKPOLY.lsp)**

Command: SHRINKPOLY (Standar) atau SHRINKPOLY2 (Dengan validasi area)



Fungsi: Menutup (close) polyline yang ujungnya masih terbuka, lalu membuat duplikat polyline baru yang ukurannya lebih kecil (ter-offset ke dalam). Hasil shrink akan otomatis masuk ke layer SHRINKED (warna Cyan).



Cara Pakai: Pilih polyline, masukkan jarak offset (shrink). Script akan otomatis memproses secara massal.



**4. Block to Text Auto-Pairing V4 (BF2T.lsp)**

Command: BF2Tv4



Fungsi: Memindahkan (Snap) objek Text/MText agar posisinya pas di titik tengah (Insertion Point) Block Reference terdekat.



Cara Pakai: Seleksi sekumpulan Block dan Text, masukkan radius toleransi pencarian. Jika ada Block/Text yang tidak punya pasangan, script akan menggambar kotak merah (LWPOLYLINE) sebagai penanda.



**5. Text to MText Enhanced (T2M (TEXT TO MTEXT ENHANCE).lsp)**

Command: T2M



Fungsi: Mengubah objek TEXT biasa menjadi MTEXT, sekaligus menambahkan baris spasi kosong (Enter) di atas atau di bawah teks tersebut. Mempertahankan warna, layer, tinggi, rotasi, dan justification teks asli.



**6. Draw Line From Text (DrawLineAJa.lsp)**

Command: DrawLineAJa



Fungsi: Menggambar garis (LINE) yang titik awalnya terkunci tepat di tengah-tengah objek teks yang dipilih.



Cara Pakai: Pilih Teks, lalu klik satu titik sembarang sebagai ujung akhir garis.



**7. Add Text Above (AddTextAbove4.lsp)**

Command: AddTextAbove



Fungsi: Menyisipkan objek Teks baru bertuliskan "NN" persis di atas teks yang sudah ada, dengan posisi center (rata tengah) dan jarak offset yang bisa diatur.



**8. Sum Aligned Dimensions (S3-Sum Numbers Autocad.lsp)**

Command: S3



Fungsi: Menjumlahkan total nilai jarak (measurement value) dari sekumpulan objek Aligned Dimension yang dipilih. Hasil total akan ditampilkan di Command Line.



**9. Multi-Copy Layout (CopyLayout3.lsp)**

Command: CopyLayout



Fungsi: Menggandakan (duplicate) sebuah tab Layout yang sudah ada menjadi beberapa copy sekaligus.



Cara Pakai: Ketik command, masukkan nama layout yang mau di-copy, lalu masukkan jumlah salinan yang diinginkan.

