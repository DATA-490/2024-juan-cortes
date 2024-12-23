# CFO Office Sign-In Dashboard
* **Author**: Juan Cortes, github: [NOTJUANCORTES](https://github.com/NOTJUANCORTES)
* **Major**: Mathematics: Statistics
* **Year**: 2024

# Type of project

This project is a web application developed using Shiny, an R package for building interactive web applications, enabling dynamic data presentation and user interaction.

# Purpose (including intended audience)

The CFO Office Sign in Sheet captures information about walk-in student visits to the Chico State campus Student Services Center(SSC) CalFresh Outreach (CFO) office.

The end goal was to create a Static Report using the Shiny app, data analyses, and final reports to keep track of visits to the office. 

This includes:
-Daily visits
-Weekly visits
-Monthly visits
-Visits by Term
-Types of Assistance
-Repeated Assistance
-How They Heard About CalFresh?

This project intended to help CFO Team by saving time and reducing manual data entry. In turn, it may also help Chico State students by supporting basic needs, which benefit low-income, 
first-generation, and underrepresented students. Lastly, this project may aid stakeholders by delivering insights regarding Federal support.



# Explanation of files

* `git-app.R` - this file includes the code necessary to create an interactive application. It includes the user interface, which defines the layout and appearance of the app, and the server, which is in-charge of creating reactive expressions, which allow the app to automatically update outputs when the inputs change.
    - Variables:
      how_can_we_assist_you_today: This variable captures what kind of assistance the student is seeking during their visit to the CalFresh Outreach office.
      how_did_you_hear_about_the_cal_fresh_office: This variable tracks how students found out about the CalFresh Outreach office.
* `Test_Data.RData` - Test data that uses 12 variables derived from the live data set.
* `www` - Folder containing the CalFresh Logo necessary for the app and the style.css file used for CSS theming.

# Completion status 

Pending steps include: 

- [ ] Fix navset_card_underline() function so that all of the tabs look the same
- [ ] Make counts and plots display data based on academic year (currently displays all-time data)

## Enhancements: 

- [ ] ​Create a Log-in
- [ ] Layout Choice

# Can someone else work on this project? 
Yes

# Public Display/dissemination
Live app URL: https://chicodatascience.shinyapps.io/CFOChico-Office-Visit-Tracker/

# License
