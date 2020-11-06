![script](https://github.com/tomasokal/dfs/workflows/script/badge.svg)

# About

The DFS Linear Optimization App uses a number of R packages and functions to generate optimial lineups for weekly daily fantasy sports contests. Each of the three main daily fantasy sports platforms (Draftkings, Fanduel, and Yahoo) are provided. Projections and salaries are continously updated throughout the week to provide up to date information. This is done through a series of [Github actions](https://github.com/tomasokal/dfs/blob/production/.github/workflows/script.yml) that scrape the [numberFire](https://www.numberfire.com/) website for player status, team, expected points, and salaries.

### Optimization process

Using the sourced projections and salaries, the application will optimally allocate the budget provided on each platform and maximize the amount of projected points using the [{lpSolve}](https://github.com/cran/lpSolve) package. This process is heavily based on previous work done by [Troy Hernandez](https://troyhernandez.com/2016/01/06/optimizing-fanduel-in-r/). 
As mentioned by Troy, a downside of this approach is that sports, the NFL, and player scoring is volatile and thus player projections can be wildly off of player performance in each game. 

To address this, our application allows for the inclusion and exclusion of players. By including or excluding players from the optimization process, the parameters f the linear programming algorithm updates to take into account remaining roster, budget, and available players.

### Points until optimal measure

Using the sourced projections and salaries, the DFS Optimization App also provides information on how close to optimal players are. Every player that is not in the default optimal lineup is given extra points up to the point at which they become optimal. This difference between projected points and points needed until optimal gives insight into how valuable each player is when taking into account the projections and salaries of all potential players.

### Disclaimer

This application is made only to provide information about sports for entertainment and educational purposes. Generated lineups or picks are not guaranteed to provide monetary gain in any bettering or daily fantasy sports contests.
