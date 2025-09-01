# White Wine Quality Classification (R Implementation)

Assessing wine quality from **physicochemical attributes** is a key task with implications for both consumer satisfaction and industrial decision-making.  
This project implements an **end-to-end binary classification pipeline in R**, covering **EDA**, Logistic Regression, Decision Trees, Ensemble methods (Bagging, Random Forest, AdaBoost), and **SVMs (Linear / Polynomial / RBF)**.  
Models are evaluated with cross-validation and **.632+ Bootstrap** to ensure robustness.

---

## Dataset

- **Name**: White Wine Quality (UCI Machine Learning Repository)  
- **File**: `winequality-white.csv` (semicolon `;` separated)  
- **Target variable recoding**:  
  - `quality.code = 1` (high quality): `quality >= 6`  
  - `quality.code = 0` (low quality): `quality <= 5`  

---

## Environment

- **R** (recommended ≥ 4.0)

**Required packages:**  
`corrplot`, `car`, `ggplot2`, `caret`, `MASS`, `boot`, `pROC`, `dplyr`, `rpart`, `rpart.plot`, `RColorBrewer`, `tidyr`, `scales`, `reshape2`, `randomForest`, `MLmetrics`, `tibble`, `purrr`, `ipred`, `ada`, `e1071`

Install all dependencies at once:
```r
install.packages(c(
  "corrplot","car","ggplot2","caret","MASS","boot","pROC","dplyr","rpart",
  "rpart.plot","RColorBrewer","tidyr","scales","reshape2","randomForest",
  "MLmetrics","tibble","purrr","ipred","ada","e1071"
))

---

## 📂 Project Structure

├── code/                         # Modular R scripts for each step
│   ├── Data import and recoding  # Load dataset and create binary labels
│   ├── EDA                       # Exploratory Data Analysis (boxplots, KDE, QQ plots, correlation)
│   ├── Logistic Regression       # Full and stepwise logistic regression with CV and bootstrap
│   ├── SVM                       # Linear, polynomial, and RBF kernels (2D & full features)
│   └── Tree-based methods        # Decision trees, Bagging, Random Forest, AdaBoost
│
├── main.R                        # Master script to run the entire pipeline
├── winequality-white.csv         # Dataset (semicolon-separated)
└── README.md                     # Project documentation
