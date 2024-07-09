# NHL Daily Fantasy Optimizer in R

Data extraction, processing, statistical analysis and lineup optimization for NHL Daily Fantasy.

## Files
*dfs_data_pipeline.R*

*  Utilize web scraper from Evolving-Hockey to extract NHL play-by-play data
*  Clean and wrangle data
*  Upload to MySQL database

*dfs_optimizer.R*

*  Load lineup file downloaded from DraftKings
*  Clean and prepare data for optimization
*  Load player data from database and compute statistics
*  Use linear programming model to generate optimal lineups for each goalie
