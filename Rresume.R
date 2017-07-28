###resume

devtools::install_github("ndphillips/VisualResume")
library(VisualResume)


VisualResume::VisualResume(
  titles.left = c("Benjamin M. Levine", "Applied Microeconomics, Data Science", "*Built with love in R using the VisualResume package"),
  titles.right = c("sites.google.com/view/benlevine", "bnlevine1@gmail.com", "Full Resume: https://www.linkedin.com/in/ben-m-levine"),
  titles.right.cex = c(2, 2, 1),
  titles.left.cex = c(4, 2, 1),
  timeline.labels = c("Education", "Skills & Interests"),
  timeline = data.frame(title = c("Pitzer College", "U.S. Department of State", "London School of Economics", "Twitter, Inc.", "Columbia University\nGraduate School of Business", "Pitzer College"),
                        sub = c("BA Student", "Fulbright Scholar - ETA", "MSc Student", "Data Scientist - Revenue Forecasting & Product Analytics", "\nPhD Candidate", "TA, Economics"),
                        start = c(2010.75, 2014.7, 2015.75, 2016.5, 2017.7,2011.7),
                        end = c(2014.3, 2015.5, 2016.9, 2017.6, 2021.4,2014.3),
                        side = c(1, 0, 1, 0, 1,1)),
  milestones = data.frame(title = c("BA", "MSc", "PhD"),
                          sub = c("Economics", "Managerial Economics & Strategy", "Quantitative Marketing"),
                          year = c(2014, 2016, 2021)),
  events = data.frame(year = c(2010.75, 2014.12, 2016.3, 2016.9),
                      title = c("Pitzer College Latino Academic Excellence Award",
                                "Harvard National Model United Nations Best Delegate",
                                "Class President, LSE MSc in Management & Strategy Group",
                                "Dissertation: Imported Product Market Competition[...]\nAwarded Distinction")),
  interests = list("Technical" = c(rep("R", 15), rep("Python", 1), rep("Stata", 15), rep("SQL",15), rep("Tableau",4)),
                   "Statistics" = c(rep("Causal Inference", 8), rep("Business\nAnalytics", 7), rep("Econometrics", 10)),
                   "Interests" = c(rep("Classic cars", 5), rep("Hiking & Backpacking", 10), rep("SE Asian cuisine", 10))),

  year.steps = 1
)


####
VisualResume::VisualResume(
  titles.left = c("Walter White, PhD", "Chemistry, Cooking, Pizza", "*Built with love in R using the InfoResume package: www.ndphillips.github.io/inforesume"),
  titles.right = c("www.lospolloshermanos.com", "TheOneWhoKnocks@gmail.com", "Full Resume: www.ndphillips.github.io"),
  titles.right.cex = c(2, 2, 1),
  titles.left.cex = c(4, 2, 1),
  timeline.labels = c("Education", "Teaching"),
  timeline = data.frame(title = c("Grinnell Col", "Ohio U", "U of Basel", "Max Planck Institute", "Old Van", "Gray Matter", "Sandia Laboratories", "J.P. Wynne High School", "A1A Car Wash"),
                        sub = c("BA. Student", "MS. Student", "PhD. Student", "PhD. Researcher", "Methamphetamine Research", "Co-Founder", "Chemist", "Chemistry Teacher", "Co-Owner"),
                        start = c(1976, 1980.1, 1982.2, 1985, 1996.5, 1987, 1991, 1995, 2001),
                        end = c(1980, 1982, 1985, 1987, 1998, 1992, 1995, 1998, 2003),
                        side = c(1, 1, 1, 1, 1, 0, 0, 0, 0)),
  milestones = data.frame(title = c("BA", "MS", "PhD"),
                          sub = c("Mathematics", "Chemistry", "Chemistry"),
                          year = c(1980, 1982, 1985)),
  events = data.frame(year = c(1985, 1995, 1997, 1999, 2012),
                      title = c("Contributed to Nobel Prize winning experiment.",
                                "Honorary mention for best Chemistry teacher of the year.",
                                "Created Blue Sky, the most potent methamphetamine ever produced.",
                                "Made first $1,000,000.",
                                "White, W., & Pinkman, J. (2012). Blue Sky: A method of [...].\nJournal of Psychopharmical Substances, 1(1),.")),
  interests = list("programming" = c(rep("R", 10), rep("Python", 1), rep("JavaScript", 2), "MatLab"),
                   "statistics" = c(rep("Decision Trees", 10), rep("Bayesian", 5), rep("Regression", 3)),
                   "leadership" = c(rep("Motivation", 10), rep("Decision Making", 5), rep("Manipulation", 30))),
  year.steps = 2
)