import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np
import csv
import pandas as pd
from collections import Counter
from itertools import combinations



# Load and prepare the data when the app starts
df = pd.read_csv('appic_clean.csv')
df['web_data'].fillna('', inplace=True)

def recommend_sites(favorite_appic_numbers, top_n=10, data=None):
    """
    Recommend internship sites similar to given favorites.
    Parameters:
    favorite_appic_numbers (list): List of APPIC Numbers for favorite sites.
    top_n (int): Number of recommendations to return.
    Returns:
    pd.DataFrame: DataFrame containing recommended sites with similarity scores.
    """
    # Create tfidf_matrix for the given dataframe (data) instead of using a global one
    vectorizer = TfidfVectorizer(stop_words='english', max_features=5000)
    tfidf_matrix = vectorizer.fit_transform(data['web_data'])

    # Find the row indices for the favorite APPIC numbers
    favorite_indices = data[data['APPIC Number'].isin(favorite_appic_numbers)].index.tolist()

    if not favorite_indices:
        raise ValueError("No matching APPIC numbers found in the dataset")

    # Compute the mean vector for the favorite sites
    favorite_vector = np.asarray(tfidf_matrix[favorite_indices].mean(axis=0))

    # Compute cosine similarity between the favorite vector and all other rows
    similarities = cosine_similarity(favorite_vector, tfidf_matrix).flatten()

    # Exclude the provided favorite sites from recommendations
    data['similarity_score'] = similarities
    recommendations = data[~data['APPIC Number'].isin(favorite_appic_numbers)]

    # Sort recommendations by similarity score and return the top N
    recommendations = recommendations.sort_values(by='similarity_score', ascending=False)

    # Select desired columns for output
    output_columns = [
        'APPIC Number', 'similarity_score', 'Site / Department', 
        'City', 'State', 'Country', 'Application Due Date', 'web_data']
    
    return recommendations[output_columns].head(top_n)




def recommend_appic_numbers(csv_file_path, input_appic_numbers):
    # Load the CSV file
    df = pd.read_csv(csv_file_path)

    # Ensure the search column is properly interpreted as lists
    df['searchcols'] = df['searchcols'].apply(eval)  # Evaluates the string representation of lists

    # Count co-occurrences of appic numbers
    cooccurrence_counter = Counter()

    # Populate co-occurrence counts
    for search_list in df['searchcols']:
        for appic_pair in combinations(search_list, 2):
            cooccurrence_counter[frozenset(appic_pair)] += 1

    # Calculate recommendations based on input_appic_numbers
    recommendation_scores = Counter()
    for appic_number in input_appic_numbers:
        for appic_pair, count in cooccurrence_counter.items():
            if appic_number in appic_pair:
                other_numbers = list(appic_pair - {appic_number})
                if other_numbers:
                    recommendation_scores[other_numbers[0]] += count

    # Rank and return the recommended APPIC numbers as a list of tuples (appic_number, score)
    # The score represents the frequency of co-occurrence, where higher scores indicate stronger recommendations
    ranked_recommendations = recommendation_scores.most_common()

    return ranked_recommendations
  
  
  
# Example usage:
csv_path = "requests_clean.csv"  # Replace with your file path
favorite_appic_numbers = [1177, 2265]  # Replace with your list


main_recs = recommend_sites(favorite_appic_numbers, data = df)

user_recommendations = recommend_appic_numbers(csv_path, favorite_appic_numbers)
# Extract the top 5 APPIC numbers from the ranked recommendations
top_5_user_recs = [appic_number for appic_number, score in user_recommendations[:5]]


user_recs_df = df[df['APPIC Number'].isin(top_5_user_recs)]


# Select columns from user_recs_df that match main_recs and reorder them
user_recs_selected = user_recs_df[['APPIC Number', 'similarity_score', 'Site / Department', 
                                   'City', 'State', 'Country', 'Application Due Date', 'web_data']]

# Ensure user recommendations are at the top
final_recs = pd.concat([user_recs_selected, main_recs], ignore_index=True)

print(final_recs.head())
final_recs.to_csv("merged_df.csv")


main_recs.columns
user_recs_df.columns
