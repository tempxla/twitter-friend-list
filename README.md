# twitter-friend-list
## about
This is a twitter firends list tool.

## install
    git clone https://github.com/tempxla/twitter-friend-list.git
    stack build
    stack install
## usage
At first, make a file "~/.twitter_api_keys" include twitter api keys.

    consumer_key        = "......"
    consumer_secret     = "......"
    access_token        = "......"
    access_token_secret = "......"

When you run command, the tool download followers and followings, and diff latest list.

    twitter-friend-list
If you need not download, add "diff" option.

    twitter-friend-list diff
    
