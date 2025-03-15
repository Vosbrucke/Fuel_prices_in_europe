# Fuel Prices in European Countries & Zones
Welcome to a project, where we explore fuel price variations across European countries and zones. You can interact with the data and customize your selection by following the link to the deployed app [here](https://vosbrucke.shinyapps.io/Fuel_prices_in_europe/). Have fun, and if you find the app useful, feel free to leave a star on the repo! 😊

### Example output:
![app](https://github.com/Vosbrucke/Fuel_prices_in_europe/blob/main/Price%20of%20Euro%2095%20in%20Europe%20until%2005%20August%202024.jpeg "Fuel prices example")

It was quite a shock we experienced in 2022, wasn't it? The Russian attack on Ukraine had major repercussions, affecting fuel prices universally across the countries in the chart. Or did it?

What’s the deal with Malta? At first glance, the data might seem incorrect, but Malta’s case is actually quite unique. In 2013, Malta’s government introduced a policy under which the state-owned fuel company secured long-term fuel supply agreements. Although the policy initially faced (at the time, justified) [criticism](https://timesofmalta.com/article/Fuel-price-stability-a-myth.556585), in hindsight, it effectively kept prices lower while maintaining price stability—its primary goal.

### Objective
This project was created to provide students, researchers, and the general public with an interactive, user-friendly historical overview of fuel prices across Europe.

### History
This project began in 2022 as an attempt to explain Poland’s inflation wave and the impact of fuel prices at the time. Early on, I realized that there was no easily accessible, visually appealing dataset displaying historical fuel prices across multiple countries — so I decided to build one myself. This effort culminated in research published [here](https://github.com/Vosbrucke/Poland_Pb95_prices) on GitHub.

Later, while studying abroad during the fuel crisis, I frequently revisited my research in discussions with friends and fellow students. I noticed that the way fuel prices were covered in the media often carried political bias and lacked economic context. Many people were unaware of the broader pricing dynamics without comparative data across neighboring countries. These conversations inspired me to expand the project.

However, my initial charts were becoming outdated, and I wanted a way to generate up-to-date, interactive visualizations. This led me to create a Shiny App that allowed users to select a country, fuel type, and date.

Well, at least, that was the plan! 😅

Once I built it, I quickly realized I would have to update the data manually every week—a repetitive task I wasn’t eager to maintain. But since coding allows for automation, I wondered if GitHub had a solution. That’s when I discovered GitHub Actions, and soon, I had a script running weekly to refresh the data automatically.

That problem was solved. But soon, a new idea emerged—one that would enhance the experience and make answering questions faster. The chart needed to be interactive, yet I couldn’t find a solution that met my visual expectations.

Then, in August 2024, I stumbled upon echarts4r—a package that instantly caught my attention. It allowed for beautiful, interactive charts, and it was exactly what my project needed! Before long, I had an operational site that fulfilled my original vision: a fully interactive, aesthetically pleasing fuel price dashboard.

### Phases:
1. Building a Shiny App with a static chart
2. Adding interactivity for country, fuel type, and date selection
3. Implementing GitHub Actions to automate weekly data updates
4. Enhancing interactivity by transitioning from ggplot to echarts4r

### TODOs:
* Reduce the size of the Shiny App input file to improve loading speed
* Create an interactive map version
