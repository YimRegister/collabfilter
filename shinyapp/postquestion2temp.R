div(class = 'container',
   
    div(class = 'col-sm-8',
        h2("After looking at what Facebook thinks you're interested in, how do you think Facebook is determining those interests?"),
        textAreaInput("postquestion2", "", "",rows=10,cols=50),
        actionButton("block_twopost", "Next",style="font-size:17pt;"),
        br()
    )
)