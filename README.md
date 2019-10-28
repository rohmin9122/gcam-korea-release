# GCAM-Korea

## What is GCAM-Korea

* GCAM-Korea is for building input files of GCAM, which represents the provincial energy system in Korea.
* GCAM-Korea subdivides the Korea energy system into the sixteen regions’ one. Even if the there are 17 provinces in Korea, the reason why GCAM-Korea represents 16 provinces without Sejong region, which is established in 2012, is that there is not enough data to model Sejong separately and just 0.5% of Korea population resides in Sejong.
* Some data has been pre-processed  to fit with the specific structure of [GCAM-USA](https://github.com/JGCRI/gcam-core) data.
* Other type of data is newly added by new structures such as nuclear power phase-out policy or modal share in transportation sector, etc.

|<img src="./res/map.png" width="500"><br>|<img src="./res/datasystem.png" width="500"><br>|
|:---:|:---:|
| [Fig 1. Spatial representation of GCAM-Korea](https://github.com/rohmin9122/gcam-korea-release/blob/master/res/map.png) |[Fig 2. GCAM-Korea Data Structure](https://github.com/rohmin9122/gcam-korea-release/blob/master/res/datasystem.png) |

## gcamkordata
#### R Pacakge
* Name of Package: gcamkordata
* Software required: R version 3.1.2 or higher
* Publication: Jeon, S.H, Roh M.Y. and Kim S.D (?) ??

#### How to install gcamkordata and make input files of GCAM
```R
# Note: tidyr ver.1.0.0 has some problems in this pacakge.
# So we recommend R version 3.4.4 or lower version of tidyr package.

# Install package
# devtools package must be installed on your PC
devtools::install_github("rohmin9122/gcam-korea-release")

# Start to build all XML files
# XML files will be created in <working directory>/xml
gcamkordata::driver()

# Make a specific XML file
driver(stop_after = "module_gcam.korea_batch_socioeconomics_xml")
```
#### How to simulate GCAM
1. Make XML files as following the above instruction.<br/>
2. Download [GCAM 5.1.3](https://github.com/JGCRI/gcam-core/releases).<br/>
3. Copy the all XML files and folder and then paste into <gcam-v5.1.3-Windows-Release-Package/gcamdata/xml>.<br/>
4. An [input configuration file](https://github.com/rohmin9122/gcam-korea-release/blob/master/configuration-gcamkorea.xml) is included in the working directory.<br/> 
5. Please refer to [GCAM 5.1.3 instruction](https://jgcri.github.io/gcam-doc/toc.html) for running GCAM.<br/>



## Model Results
* Some parameters are adjusted to reflect historical performance or trend of each provinces, such as fuel preference and income-elasticity in 2015 and 2020.

* The model results turned out to be compatible with historical performance. And the region-specific features are well captured.</br>

<p align="center">
  <img src="./res/trend.png" width="500"/><br>
  <a href="https://github.com/rohmin9122/gcam-korea-release/blob/master/res/trend.png">Fig 1. Reality cehck</a>
</p>



* In the building and transportation sectors, Seoul (Capital) and Gyeonggi (Bedroom suburb for Seoul) are the most energy-consuming provinces, reflecting the fact that 44% of total population resides in the two provinces.

* In the industrial sector, four regions with huge industrial complexes consume 77% of the national industrial energy. The industrial complexes include oil refineries and steel production companies, which are ranked 5th and 6th largest industry in the world, respectively.

* In Korea, base-load power plants are coal and nuclear power, most of them located along coastlines. Electricity is generated from those areas and then transmitted to metropolitans.

<p align="center">
  <img src="./res/TFC2015.png" width="500"/><br>
  <a href="https://github.com/rohmin9122/gcam-korea-release/blob/master/res/TFC2015.png">Fig 2. Total final energy consumption by sectors and fuels.<br>In the last row, electricity generation by fuels in 2015</a>
</p>

## Data Sources
<!--<style>
table {
  font-size: 12px;
  top:50%;
  left:50%;
}
</style>-->
|Data|Data Year|Source|URL|
|:-|:-:|:-|:-|
|Population|1970-2045|KOSIS, Population Projections and Summary indicators (Province)|http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1BPB002&conn_path=I3|
|GRDP|1975-2017|KOSIS, Gross regional domestic product per Capita by Provinc|http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1C65&conn_path=I3|
|Regional Energy balance|2011|KEEI|Internal infromation|
|Generation from renewable|2017|KEA, New & Renewable Energy Supply Statistics 2017|https://www.knrec.or.kr/pds/statistics_read.aspx?no=78&searchfield=&searchword=&page=1|
|Wind resource |2017|KEA, New & Renewable Energy Supply Statistics 2017|https://www.knrec.or.kr/pds/statistics_read.aspx?no=78&searchfield=&searchword=&page=1|
|Renewable capacity factor|2017|KEA, New & Renewable Energy Supply Statistics 2017|https://www.knrec.or.kr/pds/statistics_read.aspx?no=78&searchfield=&searchword=&page=1|
|Biodiesel supply| 2017|KEA, New & Renewable Energy Supply Statistics 2017|https://www.knrec.or.kr/pds/statistics_read.aspx?no=78&searchfield=&searchword=&page=1|
|Refining capacity| |KNOC|http://www.petronet.co.kr/main2.jsp|
|T&D loss factor| 1961, 2000-2017|KEPCO, Statistics of Electric Power in Korea 2017|http://home.kepco.co.kr/kepco/KO/ntcob/list.do?boardCd=BRD_000099&menuCd=FN05030103|
|Auxiliary use factor| 1961, 2000-2017|KEPCO, Statistics of Electric Power in Korea 2017|http://home.kepco.co.kr/kepco/KO/ntcob/list.do?boardCd=BRD_000099&menuCd=FN05030103|
|Nuclear plant information |1990-2018 |KHNP, Design Lifetime of Nuclear Power Plant| http://nsic.nssc.go.kr/intrst/view.do?ppleIntrstInfoSn=22|
|Value of shipment of cement and fertilizer|2011|KOSIS, Mining and Manufacturing Survey 2011|http://kosis.kr/publication/publicationThema.do?pubcode=FE |
|Vehicle energyconsumption|2010|KEEI, 에너지총조사보고서, 자동차주행거리연구|
|Vehicle Kilometer|2010|TS,KEA(상구과장님이 주신 보고서, 228페이지)|
|Vehicle Loadfactor|2010, 2015|KOTI,2018 국가교통통계연보|
|Temperature|1975-2015|KMA, Average temperature|https://data.kma.go.kr/stcs/grnd/grndTaList.do?pgmNo=70|
|Floor space|2011|MOLIT, Open Building Data|http://open.eais.go.kr/|