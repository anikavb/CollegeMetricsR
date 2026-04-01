## **CollegeMetricsR**

### **Proposal**

The CollegeMetricsR package is designed to simulate, analyze, and visualize relationships between student lifestyle habits and academic performance. The package focuses on key variables such as sleep, study time, stress levels, and extracurricular involvement, and provides tools to explore how these factors influence metrics such as GPA.

College students often havemultiple responsibilities, including academics, work, social life, and personal well-being. By understanding how these factors interact with one another, patterns can be identified that impact academic success and overall health.

While there are datasets on student life, there are a limited number of beginner-friendly tools that:

-   Structure student lifestyle data

-   Analyze relationships between variables

-   Give clear visualizations for interpretation

**CollegeMetricsR** aims to fill in this gap by providing a comprehensive set of functions that will help analyze student life data.

### **Potential Data Set**

<https://www.kaggle.com/datasets/lainguyn123/student-performance-factors/code>

The package will utilize a real-world dataset from Kaggle containing student performance factors, including study habits, sleep patterns, and external influences.

This dataset includes both numerical and categorical variables, such as hours studied, sleep

duration, parental involvement, and exam scores. Within the package, the data will be cleaned and transformed as needed to facilitate analysis of relationships between lifestyle factors and academic performance.

### **Functionality**

This package will enable users to:

-   Explore student performance data

-   Examine relationships between lifestyle variables and academic outcomes

-   Summarize key patterns in student data

-   Visualize trends related to student performance across metrics

-   Estimate academic performance based on personal habits

The package will incorporate the following functions:

-   **load_student_data(path)** – Loads the dataset from a specified file path

-   **clean_student_data(df)** – Cleans the dataset by handling missing values and formatting variables

-   **sleep_summary()** – Will summarise the average sleep hours and variability 

-   **sleep_study_relationship()** – Analyse the correlation between study time and the amount of sleep a student receives

-   **scale_exam_score(df) –** Converts exam scores into a GPA-like scale for easier interpretation

-   **grade_analysis()** – will summarize GPA/grades across students

-   **plot_sleep_vs_gpa(df)** – scatterplot of sleep vs GPA

-   **plot_stress_distribution(df)** – distribution of stress levels

-   **plot_study_vs_score(df)** – distribution of study time vs performance

-   **stress_impact(df)** – analyze the relationship between stress and GPA

-   **predict_gpa(df, new_data)** – estimate GPA using a linear model based on selected variables
