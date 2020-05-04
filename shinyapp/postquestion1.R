#forced choice Yes/No
div(class = 'container',
    
    div(class = 'col-sm-8',
        h2("Would you be willing to share your anonymized data of what Facebook thinks you're interested in?"),
        radioButtons("postquestion1","" , "",choices=c("Yes","No")),
        br(),
        actionButton("block_twopost", "Next",style="font-size:17pt;"),
        br()
    )
)