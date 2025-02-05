from flask import Flask, request, jsonify
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np
from flask_cors import CORS
import csv
from collections import Counter
from itertools import combinations


app = Flask(__name__)
CORS(app)

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
    df = df.drop_duplicates()
    df['searchcols'] = df['appic_numbers']
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
    filtered_recommendations = [rec for rec in ranked_recommendations if rec[0] not in input_appic_numbers]
    
    return filtered_recommendations
  
  
  
# Example usage:



@app.route('/recommend', methods=['POST'])
def get_recommendations():
    try:
        # Get the request data and log it
        data = request.get_json()
        print(f"Received request data: {data}")

        if not data or 'appic_numbers' not in data:
            return jsonify({
                'error': 'No appic_numbers provided in request',
                'received_data': data
            }), 400

        appic_numbers = data['appic_numbers']
        program = data['program_type']
        degree = data['degree_type']
        user_rec_status = data['user_rec_status']
        print(appic_numbers)
        print(program)
        print(degree)
        
        # Save data to CSV
        with open('requests_log.csv', mode='a', newline='') as file:
            writer = csv.writer(file)
            # Write headers if the file is empty
            if file.tell() == 0:
                writer.writerow(['appic_numbers', 'program', 'degree'])
            # Write new row
            writer.writerow([appic_numbers, program, degree])

        # Convert to integers and validate
        try:
            appic_numbers = [int(num) for num in appic_numbers]
        except (ValueError, TypeError):
            return jsonify({
                'error': 'Invalid APPIC numbers format',
                'received_numbers': appic_numbers
            }), 400

        filtered_df = df[(df[program] == 1) & (df[degree] == 1)]

        if user_rec_status == 1:
            # Filter the dataframe based on the program and degree types
    
            # Get recommendations
            recommendations = recommend_sites(appic_numbers, data=filtered_df)
    
            # USER RECOMMENDATIONS HERE!
            csv_path = "requests_log.csv"  # Replace with your file path

            user_recs = recommend_appic_numbers(csv_path, appic_numbers)
            
            top_5_user_recs = [appic_number for appic_number, score in user_recs[:5]]
            
            
            user_recs_df = df[df['APPIC Number'].isin(top_5_user_recs)]
            
            user_recs_df['similarity_score'] = "User Suggested"
            # Select columns from user_recs_df that match recommendations and reorder them
            user_recs_selected = user_recs_df[['APPIC Number', 'similarity_score', 'Site / Department', 
                                               'City', 'State', 'Country', 'Application Due Date', 'web_data']]
            
            # Ensure user recommendations are at the top
            recommendations = pd.concat([user_recs_selected, recommendations], ignore_index=True)

        if user_rec_status == 0:
            recommendations = recommend_sites(appic_numbers, data=filtered_df)

        # Convert DataFrame to dictionary for JSON response
        recommendations_dict = recommendations.to_dict(orient='records')
        for record in recommendations_dict:
            for key, value in record.items():
                if isinstance(value, (np.int64, np.int32)):
                    record[key] = int(value)
                elif isinstance(value, (np.float64, np.float32)):
                    record[key] = float(value)

        response = jsonify(recommendations_dict)
        return response

    except Exception as e:
        print(f"Error processing request: {str(e)}")
        return jsonify({
            'error': str(e),
            'type': type(e).__name__
        }), 500

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=9090, debug=True)
