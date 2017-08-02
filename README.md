# Facebook News Analysis

[![GitHub issues](https://img.shields.io/github/issues/tjpalanca/facebook-news-analysis.svg)]() [![GitHub forks](https://img.shields.io/github/forks/tjpalanca/facebook-news-analysis.svg?style=social&label=Fork)]() [![GitHub stars](https://img.shields.io/github/stars/tjpalanca/facebook-news-analysis.svg?style=social&label=Star)]()

## Motivation

The Philippines is the social media capital of the world. According to [this Huffington Post article](http://www.huffingtonpost.com/jonha-revesencio/philippines-a-digital-lif_1_b_7199924.html)[^1], "from a global average of 4.4 hours/day, the Filipino spends an average of 6.3 hours/day online via laptop and 3.3 hours/day via mobile." It's then no surprise that social media has become one of the main news sources for many Filipinos, and with a tumultuous 2016 Presidential Election, many issues have cropped up, from [the newly elected President hitting media for "biased" news](http://newsinfo.inquirer.net/784772/duterte-hits-media-for-sensationalism-bias), [introducing legislation around the spread of "fake news" on social media](http://www.philstar.com/headlines/2017/01/19/1664130/pangilinan-wants-facebook-penalized-over-fake-news), to [a campaign engineering a social media machine designed to weaponize hatred](http://www.bbc.com/news/blogs-trending-38173842).

Apart from what we can gather via investigative journalism, how much do we really know about the Philippine news landscape on social media? This series (and yes, I intend to finish this one) is intended to explore the phenomenon using data.

## Completed Analysis

### 01 - Facebook Page Extraction

* [Analysis Notebook](http://www.tjpalanca.com/static/20170207-fb-scraping.html)

We use the Facebook Graph API to extract all relevant information for 2016 in key news pages, as identified by the number of likes. We extract posts, comments, reactions, comment replies and reactions and then place them in a structured format in the SQLite Database.

### 02 - Topic Modeling

![Facebook News Landscape](https://github.com/tjpalanca/facebook-news-analysis/blob/master/figs/01-news-landscape-map.png?raw=true)

* [Blog Post](http://www.tjpalanca.com/2017/03/facebook-news-topic-modeling.html)
* [Analysis Notebook](http://www.tjpalanca.com/static/20170308-fb-topic-modeling.html)

We use Latent Dirichlet Allocation on news article unstructured text in order to uncover latent "topics" in the corpus. We then explore the overall distribution, time trends, and also the concentration of news pages on a particular topic to explore the assertion that media is "biased."

## Licensing

For details on the license and permission requests, please see the [license file](https://github.com/tjpalanca/facebook-news-analysis/blob/master/LICENSE.md).

## Author Information

**Troy James R Palanca**  
mail@tjpalanca.com  
[Blog](http://www.tjpalanca.com)  
[Facebook Page](http://www.facebook.com/tjpalanca.blog)  
[LinkedIn Profile](http://ph.linkedin.com/in/tjpalanca)   