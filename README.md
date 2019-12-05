# PFAS-Serum-Biomonitoring-
CDPH conducted biomonitoring for PFAS in selected regions in California. The relative contribution of PFAS in drinking water to body load is unknown. The goal of this project is to determine if drinking water PFAS levels are correlated measured exposure concentrations using statistical hypothesis testing.

Data from CDPH Biomonitoring CARE LA Study was scrubbed for confidential information. https://biomonitoring.ca.gov/results/projects/2876
The following changes were made to the original dataset:
•	Started with real LA County data for the 7 PFASs with a detection frequency above 65%
•	Substituted MDL/sqrt2 for ND values in the real data.
•	Synthesized fake PFAS data as:  (Fake PFAS = Real PFAS ± RandNum*SD), for which the multiplier was a randomly generated number between 0 and 1. Standard deviation was based on our real LA County data.
•	Substituted Not Detected for any synthesized values below the MDL
•	Scrambled demographic information – so that the distribution by gender/race/birthyear remain the same in the synthesized dataset, but the actual characteristics of participants are not revealed
•	Combined SPA 1 and SPA 2
•	Data will not match the distribution of our actual summary stats, but the dataset is made up of reasonable numbers.

