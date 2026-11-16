# Social Network Analysis - R practicals

Folder layout (data files stay in this `Practicals` folder, outside the numbered folders):

```
Practicals/
  congress.RData  committees.RData  senator_tweets.RData  senator_wiki.RData
  1/coding_social_networks.R
  2/working_with_apis.R
  3/modeling_a_network.R
  4/introduction_to_text_analysis.R
```

## Run

Open PowerShell (or the RStudio console) **in the Practicals folder** and run one script at a time:

```powershell
cd "D:\Study\College\B.Tech\SNA\Practicals"
Rscript 1/coding_social_networks.R
Rscript 2/working_with_apis.R
Rscript 3/modeling_a_network.R
Rscript 4/introduction_to_text_analysis.R
```

Each script prints tables and metrics on screen, saves the same text to `N/output.txt` and saves every graph
as a `.png` in its own folder (`1/`, `2/`, ...). In RStudio the graphs also appear in the Plots pane.

Packages needed (install once):

```r
install.packages(c("igraph", "ggraph", "ggplot2", "dplyr", "stringr"))
```

## What each script gives

| Script | Printed output | Graphs |
|--------|----------------|--------|
| coding_social_networks | vertices/edges, density, degree, components, clustering, top members, party assortativity, communities | shared-committee distribution, top-10 members, network (>= 3 committees), senator network |
| working_with_apis | accounts found, tweets per senator by gender, age vs account order, retweets by gender, t-tests | 3 bar/scatter charts |
| modeling_a_network | age summary by gender, correlations, age-group table, regression table with confidence intervals, interpretation of the age coefficient | box plot, scatter + trendline, coefficient plot |
| introduction_to_text_analysis | mention counts, correlation (Pearson + Spearman), average mentions by gender, top mentioned accounts | scatter, bar chart |

## Notes on the data

* "Number of social media accounts" (modeling_a_network) = how many of Twitter, Facebook and YouTube a member lists in `congress_contact` (0-3).
* The saved Twitter data has no account creation date or lifetime tweet count, so `working_with_apis` uses clearly labelled proxies (sampled tweets, user-id order) and says so in its output.
* Twitter accounts are linked to senators by the handle in `congress_contact`, or by the senator's last name in the screen name. Accounts of people who are no longer senators cannot be linked and are reported as such.

$files = @("congress.RData", "committees.RData", "senator_tweets.RData", "senator_wiki.RData")
foreach ($f in $files) {
    Write-Host "Downloading $f..."
    Invoke-WebRequest -Uri "https://dssoc.github.io/datasets/$f" -OutFile $f
}