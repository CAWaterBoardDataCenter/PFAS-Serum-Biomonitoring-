# PFAS-Serum-Biomonitoring-
CDPH conducted biomonitoring for PFAS in selected regions in California. The relative contribution of PFAS in drinking water to body load is unknown. The goal of this project is to determine if drinking water PFAS levels are correlated measured exposure concentrations using statistical hypothesis testing.

Data from CDPH Biomonitoring CARE LA Study was scrubbed for confidential information. https://biomonitoring.ca.gov/results/projects/2876
The following changes were made to the original dataset:

•	Started with real LA County data for the 7 PFASs with a detection frequency above 65% - PFOS, PFOA, PFHxS, PFDeA, PFNA, PFUdA, and MEFOSSA
•	Substituted MDL/sqrt2 for ND values in the real data.
•	Synthesized fake PFAS data as:  (Fake PFAS = Real PFAS ± RandNum*SD), for which the multiplier was a randomly generated number between 0 and 1. Standard deviation was based on our real LA County data.
•	Substituted Not Detected for any synthesized values below the MDL
•	Scrambled demographic information – so that the distribution by gender/race/birthyear remain the same in the synthesized dataset, but the actual characteristics of participants are not revealed
•	Combined SPA 1 and SPA 2
•	Data will not match the distribution of our actual summary stats, but the dataset is made up of reasonable numbers.

This scrubbed, simulated dataset is PFASSerumData.csv. Scrambling affected everything except for serum levels in SPAs, which are still somewhat representative.

For this study, public water well PFAS data from 5 geographic regions defined as Special Planning Areas (SPA1/2, SPA3, SPA4, and SPA7) were compared to the blood serum values.

The water well data was averaged and compared to the blood serum concentrations by SPAs. The graphs show that there appears to be no correlation between PFAS concentrations in blood serum and average PFAS concentrations in groundwater by SPA.

Comparison of blood serum distribution by SPA.  Plotted just the blood serum 





