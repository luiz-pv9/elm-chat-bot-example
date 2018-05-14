module BotBlueprint exposing (..)

import Bot
import Time
import Json.Decode as Decode


root : Bot.Blueprint
root =
    Bot.blueprint "root"
        |> Bot.send "Hello, %name%, I hope you're doing fine."
        |> Bot.wait (0 * Time.second)
        |> Bot.send "Let's talk about programming. I'm gonna show you two options using the `options` function."
        |> Bot.wait (0 * Time.second)
        |> Bot.label "choose_paradigm"
        |> Bot.send "What paradigm do your prefer?"
        |> Bot.options
            [ ( "Object Oriented", oopChoice )
            , ( "Functional", functionalChoice )
            ]


oopChoice : Bot.Blueprint
oopChoice =
    Bot.blueprint "oopChoice"
        |> Bot.send "That's a gret choice, but let's try again."
        |> Bot.wait (1 * Time.second)
        |> Bot.send "I'm gonna go back to the previous options using the `goTo` function."
        |> Bot.wait (2 * Time.second)
        |> Bot.goTo "choose_paradigm"


functionalChoice : Bot.Blueprint
functionalChoice =
    Bot.blueprint "functionalChoice"
        |> Bot.send "Immutability is great, isn't it?"
        |> Bot.wait (1 * Time.second)
        |> Bot.send "What do you want to do next?"
        |> Bot.options
            [ ( "Search for a repo on github", searchGithubChoice )
            , ( "Show a random picture of a cat", catPictureChoice )
            ]
        |> Bot.end


searchGithubChoice : Bot.Blueprint
searchGithubChoice =
    Bot.blueprint "search_github"
        |> Bot.ask "Tell the me username to search a repo" "github_username"
        |> Bot.request
            { url = "https://api.github.com/users/%github_username%/repos"
            , decoder = githubDecoder
            , format = \repo -> "The repo name is: " ++ repo
            }
        |> Bot.end


githubDecoder : Decode.Decoder String
githubDecoder =
    Decode.at [ "foo", "bar" ] Decode.string


catPictureChoice : Bot.Blueprint
catPictureChoice =
    Bot.blueprint "cat_picture"
        |> Bot.end
