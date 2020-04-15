div(class = 'container',
    div(class = 'col-sm-2'),
    div(class = 'col-sm-8',
        textInput("question2", "How much money (in USD) would you want to be paid to share your anonymized Facebook Ad Preference data with a company?"),
        textInput("question2b","How much money (in USD) would you want to be paid to share your de-anonymized Facebook Ad Preference data with a company?"),
        actionButton("block_three", "Next",style="font-size:17pt;"),
        br()
    )
)