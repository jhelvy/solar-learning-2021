### China data

**PV Module Cost**: Data are in the `china/wang_ndrc_data.csv` file. These were hand-copied from the slides from [Wang Bohua's presentation at CPIA](https://mp.weixin.qq.com/s/lU9TW6wEjR0Fe9cOYk221A). The the slides are the pdf file `王思成：中国光伏产业发展现状和前景展望.pdf`.

**PV Installed Capacity**: Same as module cost source.

### Germany data

**PV Module Cost**: Data are in the `digitize/germany/fraunhofer_fig2.csv` file. These data were extracted using the {digitize} package from the `digitize/germany/Fraunhofer-recent-facts-about-photovoltaics-in-germany-Fig2.png` image, which was sourced from the Fraunhofer report located at `digitize/germany/Fraunhofer_recent-facts-about-photovoltaics-in-germany.pdf`.

**PV Installed Capacity**: Data are in the `irena/irenaCumCapacityMw.csv` file and were copied from two PDFs from IRENA: `irena/IRENA_RE_Statistics_2016.pdf` and `irena/IRENA_RE_Capacity_Statistics_2021.pdf`, available for download [here](https://www.irena.org/Statistics/Download-Data). Direct links: [2016](https://www.irena.org/publications/2016/Jul/Renewable-Energy-Statistics-2016), [2021](https://www.irena.org/publications/2021/March/Renewable-Capacity-Statistics-2021)

### U.S. data

U.S. data came from three sources: [NREL](https://www.nrel.gov/), [LBNL](https://www.lbl.gov/), and [SEIA](https://www.seia.org/). In our analyses, we use the SEIA data for capacity and LBNL for cost data as they span a longer time period than the NREL data.

- LBNL data are in the `lbnl/tts_2019_summary_data_tables_0.xlsx` file. This is sourced from the [Lawrence Berkeley National Lab annual Tracking the Sun report](https://emp.lbl.gov/tracking-the-sun). Use [this link](https://emp.lbl.gov/sites/default/files/tts_2019_summary_data_tables_0.xlsx) to download the data file. The report is also included as a PDF in `lbnl/tracking_the_sun_2019_report.pdf`
- NREL data are found in the two Excel files `nrel/Data File (U.S. Solar Photovoltaic System Cost Benchmark Q1 2018 Report).xlsx` and `nrel/Data File (U.S. Solar Photovoltaic  BESS System Cost Benchmark Q1 2020 Report).xlsx`. These are sourced from the
[Q1 2018](https://data.nrel.gov/submissions/103) and [Q1 2020](https://data.nrel.gov/submissions/158) "U.S. Solar Photovoltaic System Cost Benchmark" reports, respectively. Use [this link](https://data.nrel.gov/system/files/103/Data%20File%20%28U.S.%20Solar%20Photovoltaic%20System%20Cost%20Benchmark%20Q1%202018%20Report%29.xlsx) to download the 2018 file and [this link](https://data.nrel.gov/system/files/158/Data%20File%20%28U.S.%20Solar%20Photovoltaic%20%20BESS%20System%20Cost%20Benchmark%20Q1%202020%20Report%29.xlsx) to download the 2020 file. PDFs of these reports are also included in the `nrel` folder.
- SEIA Capacity data are sourced from two SEIA reports. Years 2000 - 2004 are hand-copied from figure 2.1 of [this 2013 report](https://www.seia.org/research-resources/solar-market-insight-report-2013-year-review) and are available in the `seia/seiaEarlyYears.csv` file. Years 2005 - 2020 are from the [Solar Industry Research Data page](https://www.seia.org/solar-industry-research-data), accessed Feb. 27, 2021. Raw data available in json format from the html source code, saved as `seia/seiaCapacityChartSource.html`. json data were extracted and saved as `seia/seiaCapacity.json`.

### World data

Global PV production data by country are from Arnulf Jäger-Waldau's 2020 paper in Energies:

    Jäger-Waldau, A. (2020). Snapshot of Photovoltaics—February 2020.
    Energies, 13(4), 930. https://doi.org/10.3390/en13040930
    https://www.mdpi.com/1996-1073/13/4/930/htm

Since the raw data were not available, we used the [Engauge Digitizer](https://markummitchell.github.io/engauge-digitizer/) tool to digitize the data in Figure 1 from the paper. The paper, figure, and digitized data are available in the `digitize/production` folder.

Global installed capacity data are in the `irena/irenaCumCapacityMw.csv` file and were copied from two PDFs from IRENA: `irena/IRENA_RE_Statistics_2016.pdf` and `irena/IRENA_RE_Capacity_Statistics_2021.pdf`, available for download [here](https://www.irena.org/Statistics/Download-Data). Direct links: [2016](https://www.irena.org/publications/2016/Jul/Renewable-Energy-Statistics-2016), [2021](https://www.irena.org/publications/2021/March/Renewable-Capacity-Statistics-2021)

### Silicon data

Data on the global average price of silicon over time were sourced from Greg Nemet's book: Nemet, G. F. (2019). How solar energy became cheap: A model for low-carbon innovation. Routledge. The price data are in the file `nemet_silicon.csv`. Greg sent us the data directly.

### Exchange rates

The `exchange-rates.xlsx` file contains calculations to get the mean annual conversion factors between the RMB and USD (source: [Federal Reserve](https://www.federalreserve.gov/releases/h10/hist/dat00_ch.htm)) and the Euro and USD (source: [Federal Reserve](https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm))

### Projections

The `Solar PV projections.xlsx` file contains some projections of installed PV capacity in future years from a variety of sources.
