# twitter-friend-list
## about
This is a twitter firends list tool.

## install
This tool uses `stack`. Download the source, build and install, use commands:

    git clone https://github.com/tempxla/twitter-friend-list.git
    cd twitter-friend-list
    stack build
    stack install

## usage
At first, make a file `~/.twitter_api_keys`, which include twitter api keys.

    consumer_key        = "......"
    consumer_secret     = "......"
    access_token        = "......"
    access_token_secret = "......"

When you run this command, the tool downloads followers/followings lists, and compares old lists.

    twitter-friend-list
If you need not download, add "diff" option.

    twitter-friend-list diff

For print a list. n is a back number. (default 0, which means lastest)

    twitter-friend-list list n

For get user_id and screen_name, run:

    twitter-friend-list show screen_name
    twitter-friend-list show -i user_id
