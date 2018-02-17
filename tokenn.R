## whatever name you assigned to your created app
appname <- "rb_rtweet"

## api key (example below is not a real key)
key <- "1LuRvbeUriwkm4tgBzCSdKTAU"

## api secret (example below is not a real key)
secret <- "pSPc5YRrbcPyrZv9zGLoJvEY7lGtoiyaVw6AQh6z8P7hMw7gwr"

## create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)