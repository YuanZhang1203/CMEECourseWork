-------------Basic information-------------
Title: Miniproject
Author: Yuan Zhang
Time: March 2020


-----Language used and related package-----
Bash 3.2 : A file $compile.sh$ written in bash was used to compile the report into pdf format with bibliography. Another file run_miniproject.sh writtern in bash aimed to run all the files and get the final results and report.

R 3.6.1: It was applied to get the data prepartion, calculate the criteria value, as well as plot the results. Some helpful packages were used. 
dplyr was used to manage tables, 
ggplot2 was used for plotting, 
minpack.lm was used to conduct non-linear models. 


------------------ Content ----------------

├── Code
│   ├── Data_preparation.R
│   ├── Materials
│   │   ├── figure1.jpg
│   │   ├── figure4.jpg
│   │   └── logo.jpg
│   ├── Miniproject.bib
│   ├── Miniproject.tex
│   ├── Model_fitting.R
│   ├── compile.sh
│   ├── results_plotting.R
│   └── run_miniproject.sh
├── Data
│   ├── LogisticGrowthData.csv
│   ├── LogisticGrowthMetaData.csv
│   ├── modified_data.csv
│   ├── statistic_data.csv
│   └── statistic_results.csv
├── Results
│   ├── Bestfitting.png
│   ├── Fitting_percentage.png
│   ├── Miniproject.pdf
│   └── respective_plots
│       ├── 100:Staphylococcus\ spp._2_Salted\ Chicken\ Breast.png
│       ├── 101:Staphylococcus\ spp._4_Salted\ Chicken\ Breast.png
│       ├── 102:Staphylococcus\ spp._7_Salted\ Chicken\ Breast.png
│       ├── 103:Staphylococcus\ spp._10_Salted\ Chicken\ Breast.png
│       ├── 104:Staphylococcus\ spp._15_Salted\ Chicken\ Breast.png
│       ├── 105:Staphylococcus\ spp._20_Salted\ Chicken\ Breast.png
│       ├── 106:Staphylococcus\ spp._2_Cooked\ Chicken\ Breast.png
│       ├── 107:Staphylococcus\ spp._4_Cooked\ Chicken\ Breast.png
│       ├── 108:Staphylococcus\ spp._7_Cooked\ Chicken\ Breast.png
│       ├── 109:Staphylococcus\ spp._10_Cooked\ Chicken\ Breast.png
│       ├── 10:Acinetobacter.clacoaceticus.2_5_TSB.png
│       ├── 110:Staphylococcus\ spp._15_Cooked\ Chicken\ Breast.png
│       ├── 111:Staphylococcus\ spp._20_Cooked\ Chicken\ Breast.png
│       ├── 112:Pseudomonas\ spp._2_Raw\ Chicken\ Breast.png
│       ├── 113:Pseudomonas\ spp._4_Raw\ Chicken\ Breast.png
│       ├── 114:Pseudomonas\ spp._7_Raw\ Chicken\ Breast.png
│       ├── 115:Pseudomonas\ spp._10_Raw\ Chicken\ Breast.png
│       ├── 116:Pseudomonas\ spp._15_Raw\ Chicken\ Breast.png
│       ├── 117:Pseudomonas\ spp._20_Raw\ Chicken\ Breast.png
│       ├── 118:Pseudomonas\ spp._2_Salted\ Chicken\ Breast.png
│       ├── 119:Pseudomonas\ spp._4_Salted\ Chicken\ Breast.png
│       ├── 11:Stenotrophomonas.maltophilia.1_5_TSB.png
│       ├── 120:Pseudomonas\ spp._7_Salted\ Chicken\ Breast.png
│       ├── 121:Pseudomonas\ spp._10_Salted\ Chicken\ Breast.png
│       ├── 122:Pseudomonas\ spp._15_Salted\ Chicken\ Breast.png
│       ├── 123:Pseudomonas\ spp._20_Salted\ Chicken\ Breast.png
│       ├── 124:Pseudomonas\ spp._2_Cooked\ Chicken\ Breast.png
│       ├── 125:Pseudomonas\ spp._4_Cooked\ Chicken\ Breast.png
│       ├── 126:Pseudomonas\ spp._7_Cooked\ Chicken\ Breast.png
│       ├── 127:Pseudomonas\ spp._10_Cooked\ Chicken\ Breast.png
│       ├── 128:Pseudomonas\ spp._15_Cooked\ Chicken\ Breast.png
│       ├── 129:Pseudomonas\ spp._20_Cooked\ Chicken\ Breast.png
│       ├── 12:Stenotrophomonas.maltophilia.2_5_TSB.png
│       ├── 130:Aerobic\ Psychotropic._2_Raw\ Chicken\ Breast.png
│       ├── 131:Aerobic\ Psychotropic._4_Raw\ Chicken\ Breast.png
│       ├── 132:Aerobic\ Psychotropic._7_Raw\ Chicken\ Breast.png
│       ├── 133:Aerobic\ Psychotropic._10_Raw\ Chicken\ Breast.png
│       ├── 135:Aerobic\ Psychotropic._20_Raw\ Chicken\ Breast.png
│       ├── 136:Aerobic\ Psychotropic._2_Salted\ Chicken\ Breast.png
│       ├── 137:Aerobic\ Psychotropic._4_Salted\ Chicken\ Breast.png
│       ├── 138:Aerobic\ Psychotropic._7_Salted\ Chicken\ Breast.png
│       ├── 139:Aerobic\ Psychotropic._10_Salted\ Chicken\ Breast.png
│       ├── 13:Klebsiella.pneumonia_5_TSB.png
│       ├── 140:Aerobic\ Psychotropic._15_Salted\ Chicken\ Breast.png
│       ├── 141:Aerobic\ Psychotropic._20_Salted\ Chicken\ Breast.png
│       ├── 142:Aerobic\ Psychotropic._2_Cooked\ Chicken\ Breast.png
│       ├── 143:Aerobic\ Psychotropic._4_Cooked\ Chicken\ Breast.png
│       ├── 144:Aerobic\ Psychotropic._7_Cooked\ Chicken\ Breast.png
│       ├── 145:Aerobic\ Psychotropic._10_Cooked\ Chicken\ Breast.png
│       ├── 146:Aerobic\ Psychotropic._15_Cooked\ Chicken\ Breast.png
│       ├── 147:Aerobic\ Psychotropic._20_Cooked\ Chicken\ Breast.png
│       ├── 148:Aerobic\ Mesophilic._2_Raw\ Chicken\ Breast.png
│       ├── 149:Aerobic\ Mesophilic._7_Raw\ Chicken\ Breast.png
│       ├── 14:Dickeya.zeae_5_TSB.png
│       ├── 150:Aerobic\ Mesophilic._4_Raw\ Chicken\ Breast.png
│       ├── 151:Aerobic\ Mesophilic._10_Raw\ Chicken\ Breast.png
│       ├── 152:Aerobic\ Mesophilic._15_Raw\ Chicken\ Breast.png
│       ├── 153:Aerobic\ Mesophilic._20_Raw\ Chicken\ Breast.png
│       ├── 154:Aerobic\ Mesophilic._2_Salted\ Chicken\ Breast.png
│       ├── 155:Aerobic\ Mesophilic._4_Salted\ Chicken\ Breast.png
│       ├── 156:Aerobic\ Mesophilic._7_Salted\ Chicken\ Breast.png
│       ├── 157:Aerobic\ Mesophilic._10_Salted\ Chicken\ Breast.png
│       ├── 158:Aerobic\ Mesophilic._15_Salted\ Chicken\ Breast.png
│       ├── 159:Aerobic\ Mesophilic._20_Salted\ Chicken\ Breast.png
│       ├── 15:Pectobacterium.carotovorum.subsp..Carotovorum.Pcc2_5_TSB.png
│       ├── 160:Aerobic\ Mesophilic._2_Cooked\ Chicken\ Breast.png
│       ├── 161:Aerobic\ Mesophilic._4_Cooked\ Chicken\ Breast.png
│       ├── 162:Aerobic\ Mesophilic._7_Cooked\ Chicken\ Breast.png
│       ├── 163:Aerobic\ Mesophilic._10_Cooked\ Chicken\ Breast.png
│       ├── 164:Aerobic\ Mesophilic._15_Cooked\ Chicken\ Breast.png
│       ├── 165:Aerobic\ Mesophilic._20_Cooked\ Chicken\ Breast.png
│       ├── 166:Spoilage_8_Vacuum\ Beef\ Striploins.png
│       ├── 167:Escherichia\ coli_8_Vacuum\ Beef\ Striploins.png
│       ├── 168:Salmonella\ Typhimurium_8_Vacuum\ Beef\ Striploins.png
│       ├── 169:Spoilage_10_Vacuum\ Beef\ Striploins.png
│       ├── 16:Pantoea.agglomerans..RDA.R._5_TSB.png
│       ├── 170:Escherichia\ coli_10_Vacuum\ Beef\ Striploins.png
│       ├── 171:Salmonella\ Typhimurium_10_Vacuum\ Beef\ Striploins.png
│       ├── 172:Spoilage_10_C02\ Beef\ Striploins.png
│       ├── 173:Escherichia\ coli_10_C02\ Beef\ Striploins.png
│       ├── 174:Spoilage_12_Vacuum\ Beef\ Striploins.png
│       ├── 175:Escherichia\ coli_12_Vacuum\ Beef\ Striploins.png
│       ├── 176:Salmonella\ Typhimurium_12_Vacuum\ Beef\ Striploins.png
│       ├── 177:Spoilage_12_C02\ Beef\ Striploins.png
│       ├── 178:Escherichia\ coli_12_C02\ Beef\ Striploins.png
│       ├── 179:Salmonella\ Typhimurium_12_C02\ Beef\ Striploins.png
│       ├── 17:Dickeya.zeae..RDA.R._5_TSB.png
│       ├── 180:Spoilage_15_Vacuum\ Beef\ Striploins.png
│       ├── 181:Escherichia\ coli_15_Vacuum\ Beef\ Striploins.png
│       ├── 182:Salmonella\ Typhimurium_15_Vacuum\ Beef\ Striploins.png
│       ├── 183:Spoilage_15_C02\ Beef\ Striploins.png
│       ├── 184:Escherichia\ coli_15_C02\ Beef\ Striploins.png
│       ├── 185:Salmonella\ Typhimurium_15_C02\ Beef\ Striploins.png
│       ├── 186:Spoilage_20_Vacuum\ Beef\ Striploins.png
│       ├── 187:Escherichia\ coli_20_Vacuum\ Beef\ Striploins.png
│       ├── 188:Salmonella\ Typhimurium_20_Vacuum\ Beef\ Striploins.png
│       ├── 189:Spoilage_20_C02\ Beef\ Striploins.png
│       ├── 18:Acinetobacter.clacoaceticus..RDA.R._5_TSB.png
│       ├── 190:Escherichia\ coli_20_C02\ Beef\ Striploins.png
│       ├── 191:Salmonella\ Typhimurium_20_C02\ Beef\ Striploins.png
│       ├── 192:Spoilage_30_Vacuum\ Beef\ Striploins.png
│       ├── 193:Escherichia\ coli_30_Vacuum\ Beef\ Striploins.png
│       ├── 194:Salmonella\ Typhimurium_30_Vacuum\ Beef\ Striploins.png
│       ├── 195:Spoilage_30_C02\ Beef\ Striploins.png
│       ├── 196:Escherichia\ coli_30_C02\ Beef\ Striploins.png
│       ├── 197:Salmonella\ Typhimurium_30_C02\ Beef\ Striploins.png
│       ├── 198:Serratia\ marcescens_6_Pasteurised\ Skim\ Milk.png
│       ├── 199:Serratia\ marcescens_6_UHT\ Skim\ Milk.png
│       ├── 19:Stenotrophomonas.maltophilia..RDA.R._5_TSB.png
│       ├── 1:Chryseobacterium.balustinum_5_TSB.png
│       ├── 200:Serratia\ marcescens_6_Pasteurised\ Full-fat\ Milk.png
│       ├── 201:Serratia\ marcescens_6_UHT\ Full-fat\ Milk.png
│       ├── 202:Serratia\ marcescens_10_Pasteurised\ Full-fat\ Milk.png
│       ├── 203:Serratia\ marcescens_15_UHT\ Full-fat\ Milk.png
│       ├── 204:Serratia\ marcescens_6_Pasteurised\ Double\ Cream.png
│       ├── 205:Serratia\ marcescens_6_UHT\ Double\ Cream.png
│       ├── 206:Serratia\ marcescens_15_Pasteurised\ Double\ Cream.png
│       ├── 207:Serratia\ marcescens_15_UHT\ Double\ Cream.png
│       ├── 208:Arthrobacter\ sp.\ 77_0_TGE\ agar.png
│       ├── 209:Arthrobacter\ sp.\ 77_7_TGE\ agar.png
│       ├── 20:Klebsiella.pneumonia..RDA.R._5_TSB.png
│       ├── 210:Arthrobacter\ sp.\ 77_20_TGE\ agar.png
│       ├── 211:Arthrobacter\ sp.\ 77_30_TGE\ agar.png
│       ├── 212:Arthrobacter\ sp.\ 77_37_TGE\ agar.png
│       ├── 213:Arthrobacter\ sp.\ 88_0_TGE\ agar.png
│       ├── 214:Arthrobacter\ sp.\ 88_7_TGE\ agar.png
│       ├── 215:Arthrobacter\ sp.\ 88_20_TGE\ agar.png
│       ├── 216:Arthrobacter\ sp.\ 88_30_TGE\ agar.png
│       ├── 217:Arthrobacter\ sp.\ 88_37_TGE\ agar.png
│       ├── 218:Arthrobacter\ sp.\ 62_0_TGE\ agar.png
│       ├── 219:Arthrobacter\ sp.\ 62_7_TGE\ agar.png
│       ├── 21:Bacillus.pumilus..RDA.R._5_TSB.png
│       ├── 220:Arthrobacter\ sp.\ 62_20_TGE\ agar.png
│       ├── 221:Arthrobacter\ sp.\ 62_30_TGE\ agar.png
│       ├── 222:Arthrobacter\ sp.\ 62_37_TGE\ agar.png
│       ├── 223:Arthrobacter\ aurescens_0_TGE\ agar.png
│       ├── 224:Arthrobacter\ aurescens_7_TGE\ agar.png
│       ├── 225:Arthrobacter\ aurescens_20_TGE\ agar.png
│       ├── 226:Arthrobacter\ aurescens_30_TGE\ agar.png
│       ├── 227:Arthrobacter\ aurescens_37_TGE\ agar.png
│       ├── 228:Arthrobacter\ citreus_0_TGE\ agar.png
│       ├── 229:Arthrobacter\ citreus_7_TGE\ agar.png
│       ├── 22:Clavibacter.michiganensis..RDA.R._5_TSB.png
│       ├── 230:Arthrobacter\ citreus_20_TGE\ agar.png
│       ├── 231:Arthrobacter\ citreus_30_TGE\ agar.png
│       ├── 232:Arthrobacter\ citreus_37_TGE\ agar.png
│       ├── 233:Arthrobacter\ globiformis_0_TGE\ agar.png
│       ├── 234:Arthrobacter\ globiformis_7_TGE\ agar.png
│       ├── 235:Arthrobacter\ globiformis_20_TGE\ agar.png
│       ├── 236:Arthrobacter\ globiformis_30_TGE\ agar.png
│       ├── 237:Arthrobacter\ globiformis_37_TGE\ agar.png
│       ├── 238:Arthrobacter\ simplex_0_TGE\ agar.png
│       ├── 239:Arthrobacter\ simplex_7_TGE\ agar.png
│       ├── 23:Chryseobacterium.balustinum_15_TSB.png
│       ├── 240:Arthrobacter\ simplex_20_TGE\ agar.png
│       ├── 241:Arthrobacter\ simplex_30_TGE\ agar.png
│       ├── 242:Arthrobacter\ simplex_37_TGE\ agar.png
│       ├── 243:Lactobacillus\ plantarum_8_MRS\ broth.png
│       ├── 244:Lactobacillus\ plantarum_12_MRS\ broth.png
│       ├── 245:Lactobacillus\ plantarum_16_MRS\ broth.png
│       ├── 246:Lactobacillus\ plantarum_20_MRS\ broth.png
│       ├── 247:Lactobacillus\ plantarum_30_MRS\ broth.png
│       ├── 248:Weissella\ viridescens_4_MRS\ broth.png
│       ├── 249:Weissella\ viridescens_8_MRS\ broth.png
│       ├── 24:Enterobacter.sp._15_TSB.png
│       ├── 250:Weissella\ viridescens_12_MRS\ broth.png
│       ├── 251:Weissella\ viridescens_16_MRS\ broth.png
│       ├── 252:Weissella\ viridescens_20_MRS\ broth.png
│       ├── 253:Weissella\ viridescens_30_MRS\ broth.png
│       ├── 254:Lactobacillus\ sakei_4_MRS\ broth.png
│       ├── 255:Lactobacillus\ sakei_8_MRS\ broth.png
│       ├── 256:Lactobacillus\ sakei_12_MRS\ broth.png
│       ├── 257:Lactobacillus\ sakei_16_MRS\ broth.png
│       ├── 258:Lactobacillus\ sakei_20_MRS\ broth.png
│       ├── 259:Lactobacillus\ sakei_30_MRS\ broth.png
│       ├── 25:Pantoea.agglomerans.1_15_TSB.png
│       ├── 260:Oscillatoria\ agardhii\ Strain\ 97_15_Z8.png
│       ├── 261:Oscillatoria\ agardhii\ Strain\ 97_20_Z8.png
│       ├── 262:Oscillatoria\ agardhii\ Strain\ 97_25_Z8.png
│       ├── 263:Oscillatoria\ agardhii\ Strain\ 97_30_Z8.png
│       ├── 264:Oscillatoria\ agardhii\ StrainCYA\ 128_15_Z8.png
│       ├── 265:Oscillatoria\ agardhii\ StrainCYA\ 128_20_Z8.png
│       ├── 266:Oscillatoria\ agardhii\ StrainCYA\ 128_25_Z8.png
│       ├── 267:Oscillatoria\ agardhii\ StrainCYA\ 128_30_Z8.png
│       ├── 268:Pseudomonas\ sp._15_APT\ Broth.png
│       ├── 269:Pseudomonas\ sp._12_APT\ Broth.png
│       ├── 26:Pantoea.agglomerans.2_15_TSB.png
│       ├── 270:Pseudomonas\ sp._6_APT\ Broth.png
│       ├── 271:Pseudomonas\ sp._2_APT\ Broth.png
│       ├── 272:Lactobaciulus\ plantarum_10_MRS.png
│       ├── 273:Lactobaciulus\ plantarum_15_MRS.png
│       ├── 274:Lactobaciulus\ plantarum_20_MRS.png
│       ├── 275:Lactobaciulus\ plantarum_25_MRS.png
│       ├── 27:Bacillus.pumilus_15_TSB.png
│       ├── 28:Clavibacter.michiganensis_15_TSB.png
│       ├── 29:Pseudomonas.fluorescens.1_15_TSB.png
│       ├── 2:Enterobacter.sp._5_TSB.png
│       ├── 30:Pseudomonas.fluorescens.2_15_TSB.png
│       ├── 31:Acinetobacter.clacoaceticus.1_15_TSB.png
│       ├── 32:Acinetobacter.clacoaceticus.2_15_TSB.png
│       ├── 33:Stenotrophomonas.maltophilia.1_15_TSB.png
│       ├── 34:Stenotrophomonas.maltophilia.2_15_TSB.png
│       ├── 35:Klebsiella.pneumonia_15_TSB.png
│       ├── 36:Dickeya.zeae_15_TSB.png
│       ├── 37:Pectobacterium.carotovorum.subsp..Carotovorum.Pcc2_15_TSB.png
│       ├── 38:Pantoea.agglomerans..RDA.R._15_TSB.png
│       ├── 39:Dickeya.zeae..RDA.R._15_TSB.png
│       ├── 3:Pantoea.agglomerans.1_5_TSB.png
│       ├── 40:Acinetobacter.clacoaceticus..RDA.R._15_TSB.png
│       ├── 41:Stenotrophomonas.maltophilia..RDA.R._15_TSB.png
│       ├── 42:Klebsiella.pneumonia..RDA.R._15_TSB.png
│       ├── 43:Bacillus.pumilus..RDA.R._15_TSB.png
│       ├── 44:Clavibacter.michiganensis..RDA.R._15_TSB.png
│       ├── 45:Chryseobacterium.balustinum_25_TSB.png
│       ├── 46:Enterobacter.sp._25_TSB.png
│       ├── 47:Pantoea.agglomerans.1_25_TSB.png
│       ├── 48:Pantoea.agglomerans.2_25_TSB.png
│       ├── 49:Bacillus.pumilus_25_TSB.png
│       ├── 4:Pantoea.agglomerans.2_5_TSB.png
│       ├── 50:Clavibacter.michiganensis_25_TSB.png
│       ├── 51:Pseudomonas.fluorescens.1_25_TSB.png
│       ├── 52:Pseudomonas.fluorescens.2_25_TSB.png
│       ├── 53:Acinetobacter.clacoaceticus.1_25_TSB.png
│       ├── 54:Acinetobacter.clacoaceticus.2_25_TSB.png
│       ├── 55:Stenotrophomonas.maltophilia.1_25_TSB.png
│       ├── 56:Stenotrophomonas.maltophilia.2_25_TSB.png
│       ├── 57:Klebsiella.pneumonia_25_TSB.png
│       ├── 58:Dickeya.zeae_25_TSB.png
│       ├── 59:Pectobacterium.carotovorum.subsp..Carotovorum.Pcc2_25_TSB.png
│       ├── 5:Bacillus.pumilus_5_TSB.png
│       ├── 60:Pantoea.agglomerans..RDA.R._25_TSB.png
│       ├── 61:Dickeya.zeae..RDA.R._25_TSB.png
│       ├── 62:Acinetobacter.clacoaceticus..RDA.R._25_TSB.png
│       ├── 63:Stenotrophomonas.maltophilia..RDA.R._25_TSB.png
│       ├── 64:Klebsiella.pneumonia..RDA.R._25_TSB.png
│       ├── 65:Bacillus.pumilus..RDA.R._25_TSB.png
│       ├── 66:Clavibacter.michiganensis..RDA.R._25_TSB.png
│       ├── 67:Chryseobacterium.balustinum_35_TSB.png
│       ├── 68:Enterobacter.sp._35_TSB.png
│       ├── 69:Pantoea.agglomerans.1_35_TSB.png
│       ├── 6:Clavibacter.michiganensis_5_TSB.png
│       ├── 70:Pantoea.agglomerans.2_35_TSB.png
│       ├── 71:Bacillus.pumilus_35_TSB.png
│       ├── 72:Clavibacter.michiganensis_35_TSB.png
│       ├── 73:Pseudomonas.fluorescens.1_35_TSB.png
│       ├── 74:Pseudomonas.fluorescens.2_35_TSB.png
│       ├── 75:Acinetobacter.clacoaceticus.1_35_TSB.png
│       ├── 76:Acinetobacter.clacoaceticus.2_35_TSB.png
│       ├── 77:Stenotrophomonas.maltophilia.1_35_TSB.png
│       ├── 78:Stenotrophomonas.maltophilia.2_35_TSB.png
│       ├── 79:Klebsiella.pneumonia_35_TSB.png
│       ├── 7:Pseudomonas.fluorescens.1_5_TSB.png
│       ├── 80:Dickeya.zeae_35_TSB.png
│       ├── 81:Pectobacterium.carotovorum.subsp..Carotovorum.Pcc2_35_TSB.png
│       ├── 82:Pantoea.agglomerans..RDA.R._35_TSB.png
│       ├── 83:Dickeya.zeae..RDA.R._35_TSB.png
│       ├── 84:Acinetobacter.clacoaceticus..RDA.R._35_TSB.png
│       ├── 85:Stenotrophomonas.maltophilia..RDA.R._35_TSB.png
│       ├── 86:Klebsiella.pneumonia..RDA.R._35_TSB.png
│       ├── 87:Bacillus.pumilus..RDA.R._35_TSB.png
│       ├── 88:Clavibacter.michiganensis..RDA.R._35_TSB.png
│       ├── 89:Tetraselmis\ tetrahele_5_ESAW.png
│       ├── 8:Pseudomonas.fluorescens.2_5_TSB.png
│       ├── 90:Tetraselmis\ tetrahele_8_ESAW.png
│       ├── 91:Tetraselmis\ tetrahele_16_ESAW.png
│       ├── 93:Tetraselmis\ tetrahele_32_ESAW.png
│       ├── 94:Staphylococcus\ spp._2_Raw\ Chicken\ Breast.png
│       ├── 95:Staphylococcus\ spp._4_Raw\ Chicken\ Breast.png
│       ├── 96:Staphylococcus\ spp._7_Raw\ Chicken\ Breast.png
│       ├── 97:Staphylococcus\ spp._10_Raw\ Chicken\ Breast.png
│       ├── 98:Staphylococcus\ spp._15_Raw\ Chicken\ Breast.png
│       ├── 99:Staphylococcus\ spp._20_Raw\ Chicken\ Breast.png
│       └── 9:Acinetobacter.clacoaceticus.1_5_TSB.png
└── readme.md

5 directories, 292 files
