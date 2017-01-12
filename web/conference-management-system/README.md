# SigBovik Conference Management System

## Setup

### Stack
We are using [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
to build the project and manage its dependencies. You'll need stack to work on this
project.

### Sqlite
We are using a Sqlite database. You'll have to have sqlite3 installed in order
to run the project.

### Yesod
We are using a web framework for Haskell called [Yesod](http://www.yesodweb.com/).
You can follow the instructions [here](http://www.yesodweb.com/page/quickstart) to
get started with Yesod.

### Installing yesod and friends
After cloning the repo, you'll need to install all the dependencies.

  * First, run `stack build yesod-bin cabal-install --install-ghc` to install the
    yesod dev tools.
  * Next, run `stack build` to build all the libraries.

### env file
We store configurations for our environment variables in a .env file. You'll
need your .env file to contain the following fields:

  * GOOGLE_USERNAME="email goes here (for sending registration confirmations)"
  * GOOGLE_PASSWORD="password goes here"
  
## Running Locally
Yesod makes it really easy to run the project locally. All you'll need to do is run
`stack exec yesod devel`, and then visit [http://localhost:3000/](http://localhost:3000/).
