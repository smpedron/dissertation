## Post-Hoc Power Analysis, Voter Fraud Paper
## Stephanie Pedron (pedron.2@osu.edu)

library(cjpowR)

## My final sample was 752
cjpowr_amce(amce = 0.05, power = NULL, n = 752 * 10, levels = 2) # 0.99
cjpowr_amce(amce = 0.05, power = NULL, n = 752 * 10, levels = 3) # 0.94
cjpowr_amce(amce = 0.05, power = NULL, n = 752 * 10, levels = 6) # 0.70
