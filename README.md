# APPIC Site Recommender

## Project Description
The **APPIC Site Recommender** is a powerful tool designed to help users identify APPIC internship sites that closely resemble their selections. By leveraging a cosine similarity algorithm, the app analyzes site descriptions to recommend similar sites. Additionally, the app incorporates recommendations from previous users. For instance, if a prior user searched sites 1, 2, 3, 4, and 5, and a current user searches sites 3, 4, and 5, the system will recommend sites 1 and 2.

To access the app for free online, visit to [https://ewokozwok.shinyapps.io/appic](https://ewokozwok.shinyapps.io/appic).

To support my work, use the Buy Me A Coffee button to donate!


## Installation Instructions
### Step 1: Clone the Repository
Download the repository and extract it to your desktop.

### Step 2: Set Up Conda Environment
1. Install Anaconda from [https://www.anaconda.com/](https://www.anaconda.com/).
2. Navigate to the project directory in your terminal:
   ```bash
   cd /path/to/repo
   ```
3. Create the Conda environment using the provided `environment.yml` file:
   ```bash
   conda env create -f environment.yml -n appic
   ```
4. Activate the environment:
   ```bash
   conda activate appic
   ```

### Step 3: Run the Flask App
Run the recommender app using the following command:
```bash
python recommender_flask.py
```
The Flask app will run on port 9090.

### Step 4: Configure Firewall Exceptions
To access the app, you must allow incoming connections on port 9090. Below are instructions for enabling this on Windows 11:
1. Open the **Start Menu** and search for **Windows Defender Firewall with Advanced Security**.
2. Select **Inbound Rules** and click **New Rule**.
3. Choose **Port** and click **Next**.
4. Select **TCP** and specify port 9090.
5. Allow the connection and assign it to the necessary network types.
6. Finish the configuration and test the app access.

### Step 5: Run the App Locally in RStudio
1. Open RStudio and execute the following commands:
   ```r
   install.packages("remotes")
   library(remotes)
   install_github("EwokOzwok/appicrecommender")
   run_app(options = list(port = 3838))
   ```

## Usage Instructions
For a demonstration, view the "Usage Example.gif" included in the repository.

## Features
- Cosine similarity-based site recommendations
- User-driven recommendation system based on previous searches
- Compatibility with Flask and R environments

## Project Status
This project is stable.

## Contributing
Contributions are welcome! Please reach out via email before submitting pull requests.

## License
This project is licensed under the Apache License.

## Credits & Acknowledgments
Created by Evan E. Ozmat.

## Contact Information
For questions, feedback, or support, please contact: 
- Email: eozmat@albany.edu
