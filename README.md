<!-- README.md is generated from README.Rmd. Please edit that file -->
tm1r <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/R_logo.svg/1280px-R_logo.svg.png" align="right" width="60" height="60" /><img src="https://www.cubus-pm.com/sites/default/files/styles/image/public/images/grafiken/grafik_technologie_ibm_d_694x500px.jpg" align="right" width="70" height="60" />
===============================================================================


Overview
--------

Useful functions to connect to 'TM1' <https://www.ibm.com/uk-en/marketplace/planning-and-analytics> instance from R via REST API. With the functions in the package, data can be imported from 'TM1' via mdx view or native view, data can be sent to 'TM1', processes and chores can be executed, and cube and dimension metadata information can be taken. 

Installation
------------

`install.packages("tm1r")`

Usage
-----

Start with creating a connection object with `tm1_connection` function.
Then send this object to any other function that you want to work with.
