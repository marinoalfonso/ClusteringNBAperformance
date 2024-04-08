# Clustering NBA players based on performance

Welcome to the NBA Players Performance Analysis project! In this project, we delve into the world of basketball to analyze and understand the performance of NBA players over the years. Through advanced data science techniques, particularly cluster analysis, we aim to uncover patterns and insights that transcend traditional player positions.

## Project Overview
The NBA has a rich history spanning decades, with players of various skills, styles, and physical attributes gracing the courts. In this project, we leverage data spanning from 1950 to 2021 to gain deeper insights into player performance. Unlike traditional approaches that categorize players based solely on their positions (such as point guards, shooting guards, etc.), we employ cluster analysis to group players based on their overall performance metrics. This allows us to uncover hidden similarities and differences among players that may not be evident when considering positions alone.

## Results
The analysis identified three different groups of athletes, each characterized by different technical propensities:
* The first cluster represents ***Scoring Leaders***, or those players who stand out for their ability to make baskets. They present an average of more than 20 points per game and, because of their ability to make baskets from both the perimeter and the pitturato zone, they pose a constant threat to opposing defenses. In addition to this, because of their advanced game vision, they are also able to distribute a significant number of assists to generate open shots for teammates. Despite their focus on offense, Scoring Leaders contribute significantly to the defensive end by capturing rebounds, stealing balls, and blocking shots. Their presence on the team provides a complete offensive option, capable of scoring points in a variety of situations and engaging teammates in team play. The group is characterized by the presence of so-called "Superstars", those players whose significant impact on the game makes them the most important to their respective teams.

* Cluster two is that of ***Perimeter Facilitators***, which consists mainly of playmakers or guards. They are distinguished by their ability to create play from the perimeter, that is, they are able to operate effectively in the area of the court between the three-point line and the baseline. In particular, they have a good command of the ball and a low incidence of fumbled balls, so they stand out for their ability to make smart decisions during defensive pressure situations, both in terms of possession and movement without the ball. Their presence on the team will help set the pace of the game, facilitate ball movement and provide an offensive threat from the perimeter.

* The third cluster represents the ***Painted Protectors***, or those whose job is to defend the "painted" zone near the basket. These are players with defensive aptitudes who excel in capturing rebounds and blocking opposing shots, and in fact the group consists mainly of centers and big wings. They are able to beat opponents in one-on-one situations, win favorable positions under the basket and dominate the rebound battle. Their physical presence creates space for teammates and imposes strong pressure on opponents. Consequently, they are also very effective in scoring points near the basket due to their finalization skills and physical strength, as confirmed by the presence of "big stretch" players such as Embiid and Davis. These players are instrumental in creating a strong presence in the area and establishing physical supremacy in both offense and defense.
![Rplot](https://github.com/marinoalfonso/ClusteringNBAperformance/assets/166382565/c6053aef-7d38-42c9-b713-d929bde0e6f6)

## About Dataset
This dataset has been obtained from [Basketball-reference](https://www.basketball-reference.com)

**Note:** Some of the advanced performance statistics may be missing in older data.
The N.B.A. introduced the 3-point shot in the 1979-80 season. So, 3PA, 3P, and 3P% are absent in older records.
Attribute Information:

**player_data.csv**

* From -- First year
* To -- Last year
* Pos -- Position
* Ht -- Height
* Wt -- Weight
* Birth Date -- Birth Date
* Colleges -- College

**seasons_stats.csv** 

Over 50 performance stats,
Here is a list of Columns in this file and their descriptions, [Glossary](https://www.basketball-reference.com/about/glossary.html)
