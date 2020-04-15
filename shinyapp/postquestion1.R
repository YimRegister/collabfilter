div(class = 'container',
    div(class = 'col-sm-2'),
    div(class = 'col-sm-8',
        textAreaInput("postquestion1", "After looking at your own Ad Preferences in the tutorial, how do you think Facebook collects your data?", ""),
        actionButton("block_twopost", "Next",style="font-size:17pt;"),
        br()
    )
)