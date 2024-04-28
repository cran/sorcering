
sorcering v1.0.1
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


