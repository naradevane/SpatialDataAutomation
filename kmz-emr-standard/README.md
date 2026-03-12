# KMZ EMR Standard by naradevane
**Technical Documentation**

**KMZ EMR Standard** is a modern web application designed to process, restructure, and apply automated styling to FTTH network designs (KMZ/KML files). This application is engineered to accelerate drafting workflows through intelligent automation features.

Version v2.0 introduces **Excel (XLSX) Export** to HPDB with Dashboard and BOQ, alongside the new **Flood Fill Auto-Correct Foldering** engine.

---

### User Guide
1. **Open Application:** Access the web app URL via your browser.
2. **Select Mode:**
    * **CLUSTER MODE:** For distribution design (FAT to Home).
    * **SUBFEEDER MODE:** For feeder/backbone design.
3. **Upload File:** Drag and drop your `.kmz` or `.kml` file into the upload area.
4. **Process:** Click the **Process File** button and wait for completion.
5. **Download:**
    * **KMZ Result:** The visually styled result file.
    * **HPDB Result:** The **Excel Workbook (.xlsx)** report containing the Dashboard and Details.
    * **BOQ Result:** The **Excel Workbook (.xlsx)** containing the automated Bill of Quantities (Cluster mode only).

---

## Key Features & Automation

### 1. Auto Correct Foldering (Flood Fill Topology)
Eliminates tedious manual drag-and-drop foldering for the drafter. The system acts like an electrical current to automatically group items based on physical intersections and logical sequences.

* **Drafter Workflow (Minimal Effort):** * You **ONLY** need to manually place the `DISTRIBUTION CABLE` and `BOUNDARY FAT` into their correct parent folder (e.g., `LINE A`, `LINE B`).
    * You can leave `POLE`, `FAT`, and `SLING WIRE` scattered outside or dumped in a single parent folder.
* **How it Works (The Magic):**
    * **Anchor Phase:** The system locks `DISTRIBUTION CABLE` as the primary anchor.
    * **Spatial Touch:** Any `POLE` or `FAT` physically touching (within a 2-meter radius) the `LINE A` cable is automatically moved into the `LINE A` folder.
    * **Sling Wire Follows:** The system automatically splits long `SLING WIRE` lines segment-by-segment and moves them into the correct folder following their attached poles.
    * **Rescue Mission:** If a pole or FAT is isolated/not touching the main cable, the system uses **Sequential ID Logic** (e.g., if P040 is missing a line, but P039 and P041 are in `LINE A`, P040 will be forced into `LINE A`).

### 2. Spatial Grouping (Auto Folder HP Cover)
Eliminates manual sorting of homepasses into FAT subfolders.

* **Dump & Go:** You do **not** need to create sub-folders for each FAT manually. Just dump **ALL** Homepass (HP) points into a single main `HP COVER` folder.
* **Auto-Folder:** The system reads polygons located in the `BOUNDARY FAT` folder.
* **Logic:** Using *Point-in-Polygon* spatial logic, each Homepass is checked. If it falls within a specific FAT polygon area, the point is **automatically distributed** into a newly generated sub-folder named after that Polygon.
* **Auto-Description:** The number of HPs within a boundary area is calculated and displayed in the polygon description.

### 3. Export HPDB (Excel Workbook)
Replaces the old CSV format with a highly structured, multi-sheet Excel file.

* **Sheet 1: `MASTER_DATA` (Dashboard & Lookup Table):**
    * Acts as the single source of truth (Database) for all coordinates and unique IDs.
    * Contains a **Dashboard** summarizing Total HP per FAT across all FDTs.
    * **Status Indicator:** Automatically flags FATs as `⚠️ UNDER` (<10 HP) or `❌ OVER` (>16 HP).
* **Sheet 2+: `FDT-XX` (Detail per FDT):**
    * Data is automatically grouped, split into separate tabs (e.g., `FDT-01`, `FDT-02`), and formatted with specific Tube/Core colors.
* **Manual Entry Required for HPDB:**
    * **FAT Street Name:** You must manually input the street name in the `FAT_STREET_NAME` column on the `MASTER_DATA` sheet. 
    * **Project Metadata:** Hardcoded regional data at the top header of `FDT-XX` sheets must be manually adjusted according to your current project.

### 4. Generate Auto BOQ (Bill of Quantities)
Uses a **Template Injection** method to automatically generate a BOQ based on KML folder structures (e.g., `LINE A FDT 1`).

* **Automated Data Extraction:**
    * **Cables:** Extracts cable capacity and Route Length directly.
    * **Sling Wire & Tektok (Looping):** Calculates and accumulates the total sling wire length and overlapped lines (tektok) per FDT.
    * **Infrastructure Count:** Automatically counts the number of FATs, FDT types, and categorizes specific Pole types (New Pole vs Existing).
* **Manual Entry Required for BOQ:**
    * **Accessories & Splice:** Items not explicitly drawn (e.g., clamps, specific closures, drop cables) must be manually calculated.

### 5. Auto Count & Sling Wire Calculation
* **Placemark Counter:** Automatically counts assets (Poles, FATs, HPs) within a folder and appends the count to the parent folder description (e.g., *"24 EXT POLE"*).
* **Sling Wire:** Calculates the cumulative length of the `SLING WIRE` path and displays it in the description.

### 6. Smart Snap (Pole Anchor)
* If assets such as **FAT** or **SLACK HANGER** are located less than 15 meters from a pole, they are **automatically snapped** to the exact pole coordinate.
* **Pole-Safe:** Poles do not snap to other poles to maintain survey accuracy.

### 7. Auto Slack Generator
* If the `SLACK HANGER` folder is empty, the system automatically creates slacks by cloning FAT points and referencing the appropriate FDT (Unique Logic).

### 8. Cable Material Calculation
In the `DISTRIBUTION CABLE` folder, the system injects calculation descriptions:
* **Formula:** `(Drawing Length + (Total Slack x 20m)) * 1.05`
* Calculates estimated physical cable requirements including a 5% tolerance and slack loops.

### 9. Smart Routing (Branch-Relative Gravity)
* **Logic:** Reorganizes cable paths to ensure they visually connect to poles and FDTs cleanly.
* **Offset:** Automatically offsets return cables to prevent overlapping lines, ensuring visual clarity in the final render.

---

## Required Naming Conventions

To ensure automation features function correctly, input KMZ files must adhere to these standards:

### 1. Capacity Format (Cable & Devices)
Do not use spaces between the number and the letter "C".

| CORRECT | INCORRECT |
| --- | --- |
| `288C` | `288 C`, `288Core` |
| `144C` | `144-C` |
| `96C` | `96 c` |
| `48C` | `48 Core` |

### 2. Pole Naming
Format: `STATUS` + `TYPE` + `HEIGHT`.
* **Examples:** `NEW POLE 7-4`, `EXISTING POLE EMR 9-4`.

### 3. Boundary Polygon
For the *Spatial Grouping* feature, ensure boundary polygons are placed inside a folder named:
* `BOUNDARY FAT` or `BOUNDARY CLUSTER`

---

## Color Standards & Styling

The system automatically applies hexadecimal color codes. **FDT (ODC)** and **Joint Closure (JC)** colors also follow their capacity.

### 1. Cables, FDT, & Joint Closure
Colors are determined based on the largest core capacity detected in the asset name/description.

| Core Capacity | Visual Color | Hex Code | Usage |
| --- | --- | --- | --- |
| **288C** | Orange | `#FFAA00` | Feeder / Main Backbone |
| **144C** | Yellow | `#FFFF00` | Feeder / High Density Dist. |
| **96C** | Red | `#FF0000` | Feeder / Distribution |
| **72C** | Dark Blue | `#0000FF` | Distribution |
| **48C** | Purple | `#AA00FF` | Distribution / Small FDT |
| **24C** | Green | `#00FF00` | End Distribution |

> **Note:**
> * **FDT/ODC** uses a *Paddle/Square* icon colored according to input capacity.
> * **Joint Closure (JC)** uses a *Circle* icon colored according to splice capacity.

### 2. Poles
Pole icon colors distinguish height and type.

| Pole Type | Icon Color | Name Criteria |
| --- | --- | --- |
| **Pole 9 Meter** | Red | `9-4`, `9-5` |
| **Pole 7M (Standard)** | Green | `7-4`, `7-5` |
| **Pole 7M (Small)** | Light Blue | `7-3` |
| **Pole 7M (Mini)** | Purple | `7-2.5` |
| **Existing (All Sizes)** | Brown | `EXISTING` |

### 3. Other Assets
* **BOUNDARY:** White Transparent Line.
* **HP COVER:** Green Icon (Home within Boundary area).
* **HP UNCOVER:** Red Icon (Home outside Boundary area).
* **SLACK HANGER:** Red Icon (Target/Circle).

---

## Output Folder Structure

The application restructures folders as follows:

### Cluster Mode

```text
CLUSTER ID / PROJECT TITLE
 |-- BOUNDARY CLUSTER
 |-- FDT (Global)
 |-- LINE A
 |    |-- BOUNDARY FAT
 |    |-- FAT
 |    |-- HP COVER (Contains subfolders per FAT)
 |    |-- HP UNCOVER
 |    |-- EXISTING POLE EMR 7-2.5
 |    |-- EXISTING POLE EMR 7-3
 |    |-- EXISTING POLE EMR 7-4
 |    |-- EXISTING POLE EMR 9-4
 |    |-- EXISTING POLE PARTNER 7-4
 |    |-- EXISTING POLE PARTNER 9-4
 |    |-- NEW POLE 7-2.5
 |    |-- NEW POLE 7-3
 |    |-- NEW POLE 7-4
 |    |-- NEW POLE 9-4
 |    |-- DISTRIBUTION CABLE
 |    |-- SLACK HANGER
 |    |-- SLING WIRE
 |-- LINE B
 |    |-- ... (same structure)
 |-- OTHERS (Unrecognized items)

```

### Subfeeder Mode

```text
SUBFEEDER ID
 |-- JOINT CLOSURE
 |-- EXISTING POLE EMR 7-2.5
 |-- EXISTING POLE EMR 7-3
 |-- EXISTING POLE EMR 7-4
 |-- EXISTING POLE EMR 7-5
 |-- EXISTING POLE EMR 9-5
 |-- EXISTING POLE EMR 9-4
 |-- EXISTING POLE PARTNER 7-4
 |-- EXISTING POLE PARTNER 9-4
 |-- NEW POLE 9-5
 |-- NEW POLE 7-4
 |-- NEW POLE 9-4
 |-- CABLE
 |-- SLACK HANGER

```

---

## Troubleshooting & Common Issues

With the introduction of the Flood Fill Auto-Correct engine, most manual sorting errors are resolved automatically. However, you might encounter issues if the spatial geometry or naming sequence is severely flawed.

**Q: Why are my Poles or FATs left behind in the "OTHERS" folder or assigned to the wrong LINE?**
* **A:** The Auto-Correct engine requires the Pole/FAT to be physically touching the `DISTRIBUTION CABLE` (within a 2-meter radius). If it is placed too far away, the system attempts a "Rescue Mission" using its Sequential ID (e.g., matching P040 with P039/P041). If both the distance is too far AND the ID sequence is broken, the asset will fail to be assigned. **Solution:** Ensure your snapping in AutoCAD/Google Earth is accurate.

**Q: Why are parts of my Sling Wire missing, not cut, or left in the original folder?**
* **A:** The system splits the long Sling Wire into smaller segments exactly where it intersects with a Pole. If the drawn Sling Wire is floating and misses the Pole by more than 2 meters, it will not be segmented and will fail the "Follow the Leader" phase.

**Q: Why are some Homepasses (HP) colored Red (HP UNCOVER) and missing from the HPDB Excel?**
* **A:** The Point-in-Polygon spatial logic dictates that any HP coordinate falling strictly outside a Polygon will be rejected. **Solution:** Adjust the vertices of your FAT boundary polygon to fully encompass all intended target homes.

**Q: Why is the BOQ or HPDB completely empty after processing?**
* **A:** The automation strictly relies on two "Anchor" folders. You must ensure that:
  1. Your boundary polygons are inside a folder exactly named `BOUNDARY FAT`.
  2. Your raw homepass points are inside a folder exactly named `HP COVER`.
  *(Case-insensitive, but spelling must be exact).*

**Q: The application freezes or lags when I click "Process File".**
* **A:** KMZ EMR Standard processes all spatial calculations (Haversine distances, polygon intersections) client-side in your browser. If your KMZ contains massive regional backbones with tens of thousands of vertices, it may take a few seconds. **Solution:** Wait for the "Processing..." spinner to complete, or use a desktop/laptop with better CPU performance.
---

*Documentation for KMZ EMR Standard by naradevane v2.0.*