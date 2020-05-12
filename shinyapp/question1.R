
div(class = 'container',
    
    div(class = 'col-sm-8',
        h2("Would you be willing the share the data you just downloaded of what Facebook thinks you're interested in if it were anonymized? "),
        h3("Please check all options you would be comfortable sharing anonymized data with."), checkboxInput("checkbox1", label = "University Researchers", value = FALSE),
        checkboxInput("checkbox2", label = "Company Marketing Teams", value = FALSE),
        checkboxInput("checkbox3", label = "Other Apps in Your Phone (send to those companies)", value = FALSE),
        checkboxInput("checkbox4", label = "Political Campaigns", value = FALSE),
        checkboxInput("checkbox5", label = "Government Organizations", value = FALSE),
        checkboxInput("checkbox6", label = "I would not be willing to share it", value = FALSE),
        textInput("writein",label="Other"),
        br(),
        actionButton("block_two", "Next",style="font-size:17pt;float;right;"),
        br()
    )
)