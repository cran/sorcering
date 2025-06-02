Version 1.2.0
===============================

Changes
-------

GENERAL CHANGES

- Introduced the new input argument RothC_dpmrpm, which expands the possibilities 
of intitial C0 calculation for RothC with the described analytical method.

- When tsteps="annually" and "model" is not NULL, so far the annual xi was calculated with annual environmental variables.
Now monthly environmental variables (via env_in) are required and the annual cycle is much better represented.

- A list of bugs has been fixed.

-------------------------------

DOCUMENTATION

- Description for RothC_dpmrpm.

-------------------------------

VIGNETTE

- Sec. 3.3.2 RothC\
    Included RothC_dpmrpm.

-------------------------------
    
BUGS

- Variable mismatch in RothC analytical calculation! Wrong calculation when model="RothC" and either calcC0 = TRUE and C0_fracts = Null, or 
calcC0 = FALSE and C0 is a scalar and C0_fracts = Null.

- Scalar C0 elements of C0_sl were not allowed. Now possible.

- When multisite = FALSE, Yasso simulations with wood-input did not run at all!
Older versions are not recommended when multisite=FALSE, model ="Yasso15" or model="Yasso20" and Cin_wood is used.

- Variable check error when model="C-Tool" or "C-Tool-org". The user needed to use an "env_in" variable of 2 columns, but only 1 column is needed.

- When model="Yasso15" or model="Yasso20", and tsteps="annually", and Cin_wood is not NULL or Cin_wood_sl is not NULL a wrong calculation has been made.

- When tsteps="annually" there might be a risk of a total crash due to a missing iteration check in connection with the numerical method, which has not yet occurred. 

CONSEQUENCES FOR USAGE OF OLDER VERSIONS
----------------------------------------

- In general: 
          - Not recommended with annual time steps!
- RothC: 
          - Works when C0_fracts is not NULL. Works when calcC0 = FALSE and C0 is not a scalar. 
- C-Tool:
          - env_in needs a dummy second column.
- Yasso15 and Yasso20:
          - Not working when using single sites and woody input.
          - Wrong results when using with woody input and annual time steps.
- All predefined models:
          - Annual time steps cycles are not considered for the calculation of xi when using annual time steps.
-------------------------------

Changes in version 1.1.0
===============================

GENERAL CHANGES

- Introduced the spin-up modelling mode. It is now possible to perform model spin-up runs for single or multiple sites. 
Along with that there are five new function arguments: spinup, t_spin, CN_spin, t_spin_sl and CN_spin_sl, the latter two allowing for varying spin-up lengths
per site.

- Removed two function arguments: t_sim and t_sim_sl. Simulation time is now completely depending on the carbon input data (Cin, Cin_wood, Cin_sl or Cin_sl_wood).
The number of rows of the carbon input data defines simulation time steps.

-------------------------------

DOCUMENTATION

- Description for arguments spinup, t_spin, CN_spin, t_spin_sl and CN_spin_sl.

- Adapted time reference changes (since t_sim and t_sim_sl are removed) to the description of arguments and values.

- New example Nr.7 with spin-up application.

- Adapted the changes to Figure 1 and Figure 2 (only vignette).

- Typo: Changed "theta_n_uncertain" to "theta_n_unc" in example Nr.5 (only vignette).

-------------------------------

BUGS

- Indexing error when model="C-Tool". env_in needed to have at least one more rows than simulated time steps. Now fixed.

- Simulations with multiple sites and C_wood_sl as input without uncertainties did not work when calcN = FALSE. Now fixed.

- Error in some warning messages where it said that the number of rows of input data must match the 'number of pools'. Fixed with 'number of time steps'.

- Error in RothC_xi_ex data. 'all( abs(out_rothC$C-out_rothC_own$C) < 1e-14)' in example 1 did not lead to output TRUE. Now fixed.


-------------------------------

Changes in version 1.0.1
===============================

GENERAL CHANGES

Only concerns RothC!

- Argument env_in: introduced new soil type class '2'\
It can be used when the soil is bare, but this only should influence the accumulated but not the maximum topsoil moisture deficit.
The latter will then be calculated as if there was soil cover.
The idea behind this is that the water content should be decisive for the microorganisms as a habitat and transport medium,
regardless of whether a plant is growing or not.

- The actual maximum topsoil moisture deficit at t=0 previously corresponded to the water flux (precipitation minus evapotranspiration)
but can now additionally not be smaller than the maximum topsoil moisture deficit (see eq. 30 in package vignette)

-------------------------------

R DOCUMENTATION

- Adapted description for argument env_in.

-------------------------------

VIGNETTE

- Sec. Arguments\
    Adapted description for argument env_in.

- Sec. 3.3.1 Yasso\
    Table1: Yasso standard parameters. Corrected p1–p12 descriptions.

- Sec. 3.3.2 RothC\
    Corrected eqs. 22 and 23.\
    Exchanged eqs. 26–30 by eqs. 26–32 to adapt new soil type '2' and corrected typos in eq. 30.\
    Simplified eq. 33 (formerly eq. 31).


