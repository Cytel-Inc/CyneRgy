# CyneRgy
R integration with Cytel products provides a highly efficient approach to achieving custom adaptive clinical trial designs, without requiring the user to develop an entire R Code base. CyneRgy is an R package to illustrate the synergy of using R and Cytel products for clinical trial simulation and provide the users with tools to help develop new functions.  


# Introduction 

This repository and corresponding website provides an overview of the R examples provided in this repository.  Each example is intended to be used with one of Cytel's products (East or Solara).  Each example is included in a directory that provides an R Studio project file, a Description file that describes the example,  RCode folder which contains the example R scripts, FillInTheBlinakRCode are the worked examples with various code deleted so the user can practice and fill in the blanks. 

In addition, the CyneRgy package provide helpful function for the user to create their own R functions based on templates and a seamless R experience for viewing example in R Studio.   

In addition, the Sandbox directory contains in progress examples that are not complete.  Please be advised that the examples in this directory have not been completed or tested. 

The following examples are included:

## Patient Simulation 

1. **[Two Arm, Normal Outcome - Patient Outcome Simulation Examples](docs/articles/2ArmNormalOutcomePatientSimulationDescription.html)**
The following examples demonstrate how to add new patient outcome simulation capabilities into East using an R function in the context of a two-arm trial with a normally distributed patient outcome. For all examples, we assume the trial design consists of standard of care and an experimental treatment and the trial design assumes patient outcomes are normally distributed.
1. **2-Arm, Binary Outcome, Patient Simulation**: These example demonstrates how to add new patient outcome simulation functionality into East using an R function in the context of a two-arm trial with normal outcomes. For all examples, we assume the trial design consists of standard of care and an experimental treatment.  The patient outcomes are normal.  The intent of these examples is to demonstrate how to add new ways to simulate patient data using R in a variety of trial examples.        

1. **2-Am, Time-to-Event Outcome, Patient Simulation**: This example demonstrates how to add new patient outcome simulation functionality into East using an R function.  For all examples, we assume the trial design consists of standard of care and an experimental treatment.  The patient outcomes are time-to-event.  The intent of these examples is to demonstrate how to add new ways to simulate time-to-event data using R.  

## Analysis 

1. **2-Arm, Binary Outcome Analysis**: This example demonstrates how to add new analysis functionality  into East using an R function.  For all examples, we assume the trial design consist of control and an experimental treatments. There are 2 Interim Analysis, IA, and a Final Analysis, FA. At the IA, the analysis is performed and depending on the example may determine early efficacy or early futility depending on the design.

## Treatment Selection 

1. **[Treatment Selection](docs/articles/TreatmentSelectionDescription.html)**: This example demonstrates a multi-arm trial with a treatment selection rule.  The examples provide additional approaches to treatment selection. 

## Advanced Example
These example provide mutiple R function to achieve a more complex design option. 

1. **2-Am, Time-to-Event Outcome, Sample Size Re-estimation**: This example demonstrates how to add new approaches to the SSR when using time-to-event data. For the SSR in this type of design, the number of events can be increased by the function in R.
