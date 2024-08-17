# Fuel prices in European countries / zones
Welcome to the project where we explore the prices of different fuel types in European countries / zones.

Find the deployed app [here](https://vosbrucke.shinyapps.io/Fuel_prices_in_europe/).

An example of what you can expect from the app :)
![app](https://github.com/Vosbrucke/Fuel_prices_in_europe/blob/main/Price%20of%20Euro%2095%20in%20Europe%20until%2005%20August%202024.jpg "Fuel prices example")

It was quite a shock we experienced in 2022 wasn't it!? The Russian attack on Ukraine had major repercussions, which were felt universally across all countries in the chart. Or were they?? What's the deal with Malta!? Indeed an interesting case! In 2013, Malta's government introduced a policy under which state-owned fuel company secures long term fuel supply agreements. The policy seems to pay off a lot although it was heavily [criticized](https://timesofmalta.com/article/Fuel-price-stability-a-myth.556585) at the beginning. Time has shown tough that it achieved it's primary goal- ensuring price stability. Pretty cool to see a well-implemented policy amid concerns having (with hindsight) such positive outcome!

### Project's objective
The project was initiated with a goal of better informing students, friends, and others interested in fuel prices by providing an approachable, interactive and web-based historical outlook on fuel prices in Europe. 

### Project's history
The work began in 2022 with the aim to explain the intricate details behind the inflation wave Poland was experiencing at the moment and how fuel prices were influencing the situation. In the intial phase of the project I realized I coulnd't find historic fuel prices across countries presented in charts and maps (also, visually appealing). So, I decided to create them myself. This effort culminated in the research published [here](https://github.com/Vosbrucke/Poland_Pb95_prices) on GitHub.

Later, while studying abroad amidst the fuel crisis, I frequently revisited my work during discussions with friends and fellow students. I discovered that the issue of bias and media coverage, which gave the topic a political slant with little economic context, was not unique to Poland. Many students and ordinary people lacked a comprehensive understanding of the dynamics behind fuel prices without a comparative outlook across neighboring countries. This led to interesting conversations and many deconstructions of ideas on both sides. However, while presenting my research on Poland, I soon found that my findings were becoming outdated. We wanted to compare how our countries fared with the most recent data! I needed to adapt the chart creation to inputs—selected country, fuel, and date. I also needed to ensure that it was accessible online in an interactive format. It was a perfect use case for learning **Shiny App**, right!? Once I created it, it worked great!

Well, I wish hah! I was satisfied until I realized that I would need to run data processing once a week... I couldn't cope with updating it manually- I'm lazy for there repetitive tasks... But, hey, it's coding so let's just learn **GitHub Actions** and automate the script to run weekly to keep the data up-to-date. With that operational, I was finally satisfied. I still recognized some limitations and realized that the graph would look better in an interactive format. I didn’t have time back then to work on it, and frankly, I had spent too long dealing with oil prices. It was cool, but I didn’t want to dive into the sector any further! So I left my loyal bot to do its thing. A year and a half passed during which I started working as a data scientist using R, learning many new and cool tools. I wanted to reformat the data processing code to **data.table** but it didn’t feel like enough to return to the project. Things changed in August when I discovered a package that instantly grabbed my attention- **echarts4r**. It creates interactive charts in a very appealing style! Soon, I had an operational site with my original objective met— a fully reactive site maintaining my desired aesthetics for the chart!

### Project's phases:
1. Building a Shiny App with a static chart
2. Adding interactivity to the chart for country/zone, fuel type and date inputs
3. Implementing GitHub Actions to run the data preparation code on a weekly basis
4. Enhancing interactivity by translating the chart from ggplot to echarts4r

### 'That's it right!? We celebrate now!? You wish!' section (aka TODOs):
* decrease the size of the Shiny App input file improving loading speed
* create an interactive map version
