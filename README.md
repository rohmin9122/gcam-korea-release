# gcam-korea-release
gcam-korea data system

## Introduction


## Model Results

<p align="center">
  <img src="/res/TFCin2015.png" width="700" height="1000"><br>
  <b>Fig1. Total final energy consumptions in 2015</b>
</p>

## Package Instructions

```R
# Install package
install.packages("gcamkordata")

# Start to make input xml files
driver()

# Make a specific xml files
driver(stop_after = "module_gcam.korea_batch_socioeconomics_xml")
```

Please download GCAM 5.1.3 release package (https://github.com/JGCRI/gcam-core/releases) and then, put the xml files on ./gcamdata/xml.
