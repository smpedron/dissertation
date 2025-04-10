// JavaScript Code for forcing respondent to stay on question for set amount of time (Qualtrics)
// Place in AddOnLoad section Qualtrics Java

    // Disable the Next button
    this.disableNextButton();

    // Timer for 15 seconds (15000 milliseconds)
    var that = this;
    setTimeout(function(){
        // Enable the Next button after 15 seconds
        that.enableNextButton();
    }, 15000);