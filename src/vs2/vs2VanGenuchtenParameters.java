/*
 * vs2VanGenuchtenParameters.java
 */
package vs2;

import java.awt.*;

/**
 * Generic soil properties used in the van Genuchten model
 */
public interface vs2VanGenuchtenParameters {

   static final String [] VG_NAME = {
     "medium sand", "fine sand",   "sandy loam",  "silt loam",   "Yolo light",
     "sand CP", "loamy sand CP", "sandy loam CP", "loam CP", "silt CP", "silt loam CP",
     "sandy clay loam CP", "clay loam CP", "silty clay loam CP", "sandy clay CP",
     "silty clay CP", "clay CP"};

   static final double [] VG_ANISOTROPY = {
      1.0,            1.0,           1.0,           1.0,           1.0,
      1.0,            1.0,           1.0,           1.0,           1.0,
      1.0,            1.0,           1.0,           1.0,           1.0,
      1.0,            1.0};

   static final double [] VG_SATURATED_HYDRAULIC_CONDUCTIVITY = {
     4.63e-3,       2.43e-5,        8.1e-6,        2.6e-6,        1.3e-7,
     8.25e-5,       4.05e-5,        1.23e-5,       2.89e-6,       6.94e-7,
     1.25e-6,       3.64e-6,        7.22e-7,       1.94e-7,       3.33e-7,
     5.56e-8,       5.56e-7};

   static final double [] VG_SPECIFIC_STORAGE = {
   0.0001,         0.0001,        0.0001,        0.0001,        0.0001,
   0.0001,         0.0001,        0.0001,        0.0001,        0.0001,
   0.0001,         0.0001,        0.0001,        0.0001,        0.0001,
   0.0001,         0.0001};

   static final double [] VG_POROSITY = {
      0.375,          0.377,         0.496,         0.43,          0.495,
      0.43,           0.41,          0.41,          0.43,          0.46,
      0.45,           0.39,          0.41,          0.43,          0.38,
      0.36,           0.38};

   static final double [] VG_RESIDUAL_MOISTURE_CONTENT = {
      0.02,           0.072,         0.15,          0.17,          0.175,
      0.045,          0.057,         0.065,         0.078,         0.034,
      0.067,          0.100,         0.095,         0.089,         0.100,
      0.07,           0.068};

   static final double [] VG_ALPHA = {
       4.31,          1.04,          .847,          0.505,          2.49,
      14.5,          12.4,          7.5,            3.6,            1.6,
      2.0,            5.9,           1.9,           1.0,            2.7,
      0.5,       0.8};

   static final double [] VG_BETA = {
      3.1,            6.9,           4.8,           7.0,           1.6,
      2.68,           2.28,          1.89,          1.56,          1.37,
      1.41,           1.48,          1.31,          1.23,          1.23,
      1.09,           1.09};
}
