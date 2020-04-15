div(class = 'container',
    div(class = 'col-sm-2'),
    div(class = 'col-sm-8',
        h2("Thanks!"),
        h3("Click Next to start exploring your data."),
        br(),
        actionButton("completed", "Next",style="font-size:17pt;"),
        br()
    )
)