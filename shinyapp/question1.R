
div(class = 'container',
    
    div(class = 'col-sm-8',
        h2("Would you be willing to share your anonymized data of what Facebook thinks you're interested in? Who would you be comfortable sharing it with? Check all that apply."),
        checkboxInput("checkbox1", label = "University Researchers", value = FALSE),
        checkboxInput("checkbox2", label = "Company Marketing Teams", value = FALSE),
        checkboxInput("checkbox3", label = "Other Apps in Your Phone (send to those companies)", value = FALSE),
        checkboxInput("checkbox4", label = "Political Campaigns", value = FALSE),
        checkboxInput("checkbox5", label = "Government Organizations", value = FALSE),
        textInput("writein",label="Other"),
        br(),
        actionButton("block_two", "Next",style="font-size:17pt;"),
        br()
    )
)