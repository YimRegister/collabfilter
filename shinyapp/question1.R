div(class = 'container',
    div(class = 'col-sm-2'),
    div(class = 'col-sm-8',
        radioButtons("question1", "Would you be willing to share your anonymized Facebook Ad Prefence data?", "",choices=c("Yes","No")),
        actionButton("block_two", "Next",style="font-size:17pt;"),
        br()
    )
)