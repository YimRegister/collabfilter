#open ended, baseline
div(class = 'container',
    
    div(class = 'col-sm-8',
        h2("How do you think Facebook comes up with what you might be interested in? List as many ways as possible."),
        h3("e.g. they gather data from what you click on"),
        textAreaInput("question2", "","",rows=10,cols=50),
        actionButton("block_three", "Next",style="font-size:17pt;"),
        br()
    )
)