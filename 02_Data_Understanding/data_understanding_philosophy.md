### Phase 02: Data Understanding

In this chapter, **we cover the next CRISP-DM Step: Data Understanding.** We take a pause with the BSPF while we **get to know the data and begin the process of preparing for modeling.** In this chapter, you will learn:

* Techniques for effectively analyzing the features in dataset
* How to use the skimr package for data investigation by data type
* How to use the GGally package for visual data investigation

### WHERE WE ARE AFTER 'BUSINESS UNDERSTANDING' PHASE

In the business understanding phase we gathered enough data to:

1. Better understand the business problem.
2. Glean insights and share with stakeholders.
3. Sized the problem to investigate if it's worth our time e.g., 1k problem vs. 1M dollar problem.

### DATA UNDERSTANDING WORKFLOW (2nd phase of CRISP-DM)

This is where we collect and dive into, exploring our data.

* **Preparation Needed Prior to beginning Phase:** Ideally, we've collected data and interatively met with stakeholders to **Understanding the Business Problem.** In the beginning it's best to start with simple data and iteratively move toward gathering, importing, and tidying your data to move towards modeling it.

### High Level Goals
1. Collect & Compile Data (engineer features, etc.)
2. Exploratory Data Analysis (EDA): Identify critical properties of the underlying features (Independent/Predictor Variables) and potential relationships between them and our Target (Response/Dependent Variable).

### Tools we will use to be Highly Effective & Efficient (EDA R Packages)
1. skimr package: Review many features by datatype.
2. GGally package: Excellent package for visualizing relationships between a Target Variable & the Features w/in our Data.

### THE WORKFLOW FOR DATA UNDERSTANDING PHASE

Buy now your data should have all the predictor features along with the target feature i.e., Tidy Data.

1. **Glimpse() Function** to View data and understand various ways features can be grouped into feature categories.
   * Question to ask: How do these features relate together (e.g., descritive features, time-based, department, etc.).
   * **Pro-Tip:** Breakdown data collection activities in to strategic areas.
2. **Exploratory Data Analysis (EDA)**
   * Separated Into Two Steps
      * EDA Part 1: Data Summarization w/skimr::skim()









