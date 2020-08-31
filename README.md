# How (Not) to Find Bugs: The Interplay Between Merge Conflicts, Co-Changes, and Bugs

-> ICSME 2020 <-

Authors

  -Luis Amaral
  -Marcos de Oliveira 
  -Welder Luz
  -José Fortes
  -Rodrigo Bonifácio
  -Daniel Alencar
  -Eduardo Monteiro
  -Gustavo Pinto
  -David Lo

Abstract

Context: In a seminal work, Ball et al. [1] investigate if the information available in version control systems could be used to predict defect density, arguing that practitioners and researchers could better understand errors “if [our] version control system could talk”. In the meanwhile, several research works have reported that conflict merge resolution is a time consuming and error-prone task, while other contributions diverge about the correlation between co-change dependencies and defect density. Problem: The correlation between conflicting merge scenarios and bugs has not been addressed before, whilst the correlation between co-change dependencies and bug density has been only investigated using a small number of case studies—which can compromise the generalization of the results. Goal: To address this gap in the literature, this paper presents the results of a comprehensive study whose goal is to understand whether or not (a) conflicting merge scenarios and (b) co-change dependencies are good predictors for bug density. Method: We first build a curated dataset comprising the source code history of 29 popular Java Apache projects and leverage the SZZ algorithm to collect the sets of bug-fixing and bug-introducing commits. We then combine the SZZ results with the set of past conflicting merge scenarios and co-change dependencies of the projects. Finally, we use exploratory data analysis and machine learning models to understand the strength of the correlation between conflict res- olution and co-change dependencies with defect density. Findings: (a) conflicting merge scenarios are not more prone to introduce bugs than regular commits, (b) there is a negligible to a small correlation between co-change dependencies and defect density— contradicting previous studies in the literature.

Index Terms

software defects, software integration, merge conflicts, co-change dependencies
