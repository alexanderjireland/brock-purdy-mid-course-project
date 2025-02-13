# brock-purdy-mid-course-project

README: Evaluating Brock Purdy’s True Skill

# Introduction
Brock Purdy’s rapid rise from the final pick of the 2022 NFL Draft to leading the 49ers to deep playoff runs in his first two seasons was nothing short of remarkable. However, after a challenging 2024 season marked by injuries to key teammates and a decline in performance, questions arise about how much of his success is due to his individual talent versus the strength of the elite system around him.

# Motivation
Brock Purdy has had an impressive start to his NFL career with the San Francisco 49ers. Selected very last in the 2022 NFL draft, Purdy had the opportunity to start for the 49ers his rookie year when their veteran quarterback Jimmy Garoppolo and first round pick Trey Lance both became injured. Purdy shot off to an impressive 5-0 in his first five games as a starter, and his success continued as Purdy brought the 49ers to the NFC championship in 2022, where he was forced off the field with an elbow injury and the 49ers ended up losing to the Philadelphia Eagles. The following season (2023) was just as impressive for Purdy, taking the 49ers all the way to the Super Bowl where San Fransisco lost to the Kansas City Chiefs by 3 points in overtime. Purdy’s first two seasons are undeniably impressive. However, the 2024 season proved challenging for Purdy, as his completion percentage, passer rating, and TD-INT ratio all declined amidst a year marked by injuries to key offensive players, most notably running back Christian McCaffrey. The 49ers ended their 2024 season with Purdy’s worst record yet, finishing 6-11.
Purdy’s early success followed by a decline raises critical questions: To what extent is his performance a reflection of his own skill versus the strength of the team around him? Purdy benefits from an elite supporting cast, each playing a crucial role in his success:
Trent Williams – Left Tackle (LT): The anchor of the offensive line, responsible for protecting Purdy’s blindside and giving him the time needed to make accurate throws.
George Kittle – Tight End (TE): A versatile player who excels in both blocking and receiving, providing a reliable target for short and intermediate passes while also contributing to pass protection.
Deebo Samuel & Brandon Aiyuk – Wide Receivers (WR): Dynamic playmakers who create separation, make tough catches, and extend plays after the catch, giving Purdy multiple downfield threats.
Christian McCaffrey – Running Back (RB): A dual-threat weapon who not only excels in rushing but also serves as a dependable receiving option, easing pressure on Purdy by keeping defenses off balance.
Purdy is further supported by his head coach, Kyle Shanahan, who is known for his elite offensive schemes. The 49ers have made it clear they intend to significantly increase their quarterback’s compensation this offseason, raising his current average salary of $934,252 to $50 million or more. This would place Purdy among the highest-paid quarterbacks in the NFL, but does his skill truly justify such a contract?
# Key Analysis Components
Metrics for measuring quarterback skill:
## Expected Points Added (EPA)


Expected Points Added (EPA) per play is a metric that quantifies the impact of each play on a team’s likelihood of scoring. The values for Expected Points (see figure above) were taken from historical data by calculating the average number of points scored by the possessing team according to each down and field position. EPA per play is simply the difference in Expected Points before and after a play. Instead of just measuring yardage, EPA accounts for game context, such as downs, distance to a first down, and field position, to determine how much a play increases or decreases a team’s expected points. For example, a 5-yard gain on a third-and-4 is much more valuable than a 5-yard gain on a third-and-10, as it results in a first down and extends the drive. EPA is an effective metric for measuring quarterback skill because it captures a quarterback’s ability to make plays that meaningfully contribute to scoring.

https://bestballstats.com/expected-points-added-a-full-explanation/

## Adjusted Net Yards an Attempt (ANY/A)
ANY/A=Pass Yards+20Pass TD-45Interceptions Thrown-Sack YardsPassing Attempts+Sacks

\text{ANY/A} = \frac{\text{Pass Yards} + 20 \times (\text{Pass TD}) - 45 \times (\text{Interceptions Thrown}) - \text{Sack Yards}}{\text{Passing Attempts} + \text{Sacks}}

Adjusted Net Yards an Attempt (ANY/A) is an advanced passing efficiency metric that improves on traditional yards per attempt by incorporating touchdowns, interceptions, and sacks, thus providing a more comprehensive measure of a quarterback’s effectiveness in the passing game. ANY/A can be a valuable tool for comparing performance across different players and seasons.

https://www.advancedfootballanalytics.com/2013/10/introducing-anya-differential.html

## Passer Rating
\begin{aligned}
    a &= \left(\frac{\text{Comp}}{\text{Att}} - 0.3\right) \times 5 \\
    b &= \left(\frac{\text{Yds}}{\text{Att}} - 3\right) \times 0.25 \\
    c &= \frac{\text{TD}}{\text{Att}} \times 20 \\
    d &= 2.375 - \left(\frac{\text{INT}}{\text{Att}} \times 25\right) \\
    \text{Passer Rating} &= \frac{a + b + c + d}{6} \times 100
\end{aligned}

\text{Note: Each component (a, b, c, d) must be between 0 and 2.375.}


	Passer rating is calculated based on completions, passing yards, touchdowns, and interceptions per attempt, generating a score between 0 and 158.3 that is designed to summarize a quarterback’s effectiveness. A higher score indicates better performance. While passer rating is useful for comparing QBs within a season, it has limitations as it does not account for sacks, rushing ability, or game context.

Combining ANY/A, EPA, and Passer Rating provides a well-rounded approach to evaluating quarterback performance, capturing efficiency (ANY/A), impact on scoring (EPA), and traditional passing effectiveness (Passer Rating).
