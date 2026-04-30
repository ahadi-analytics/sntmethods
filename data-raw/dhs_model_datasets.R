## Script to document how DHS model dataset URLs were obtained
## Source: https://dhsprogram.com/data/Download-Model-Datasets.cfm

# This script documents how the DHS model dataset URLs were obtained by
# parsing the DHS website's model datasets page.

# The URLs follow a consistent pattern:
# https://dhsprogram.com/data/model_data/{category}/{filename}
# where category is:
#   - "tables" for final reports
#   - "dhs" for standard recodes (BR, CR, HR, IR, KR, MR, PR)
#   - "hiv" for HIV test results (AR)
#   - "gps" for geographic datasets

# File naming conventions:
# - zz = Model dataset country code
# - br62, cr61, hr62, ir62, kr62, mr61, pr62, ar61 = recode types with version numbers
# - ge61, gc61 = geographic data
# - dt = Stata, sv = SPSS, sd = SAS, fl = Flat ASCII, (no suffix) = Hierarchical ASCII

# The following files are available as of extraction date:

model_files <- list(
  # Full report tables
  tables = c(
    "zzfulltables.zip",        # English
    "zztableauxcomplets.zip"    # French
  ),

  # Standard DHS recodes
  dhs = c(
    # Births Recode (BR)
    "zzbr62dt.zip", "zzbr62fl.zip", "zzbr62sd.zip", "zzbr62sv.zip",
    # Couples' Recode (CR)
    "zzcr61dt.zip", "zzcr61fl.zip", "zzcr61sd.zip", "zzcr61sv.zip",
    # Household Recode (HR)
    "zzhr62dt.zip", "zzhr62fl.zip", "zzhr62sd.zip", "zzhr62sv.zip",
    # Individual Recode (IR) - includes hierarchical
    "zzir62.zip", "zzir62dt.zip", "zzir62fl.zip", "zzir62sd.zip", "zzir62sv.zip",
    # Children's Recode (KR)
    "zzkr62dt.zip", "zzkr62fl.zip", "zzkr62sd.zip", "zzkr62sv.zip",
    # Men's Recode (MR) - includes hierarchical
    "zzmr61.zip", "zzmr61dt.zip", "zzmr61fl.zip", "zzmr61sd.zip", "zzmr61sv.zip",
    # Household Member Recode (PR)
    "zzpr62dt.zip", "zzpr62fl.zip", "zzpr62sd.zip", "zzpr62sv.zip"
  ),

  # HIV test results
  hiv = c(
    "zzar61.zip", "zzar61dt.zip", "zzar61fl.zip", "zzar61sd.zip", "zzar61sv.zip"
  ),

  # Geographic datasets
  gps = c(
    "zzge61fl.zip",     # Shapefile
    "zzgc61fl.zip",     # CSV
    "regional.zip",     # Regional boundaries shapefile
    "subregional.zip"   # Subregional boundaries shapefile
  )
)

# Total: 41 files
# - 2 final reports
# - 35 survey datasets (30 standard DHS + 5 HIV)
# - 4 geographic datasets

# The function dhs_model_datasets() in R/dhs_model_datasets.R
# provides programmatic access to these URLs with metadata