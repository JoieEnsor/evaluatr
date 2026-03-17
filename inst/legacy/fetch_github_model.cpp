#include <Rcpp.h>
#include <curl/curl.h>
#include <string>
#include <sstream>

using namespace Rcpp;

// Callback function for libcurl to write response data
static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

// Base64 decoding function (header-only implementation)
static const std::string base64_chars = 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "0123456789+/";

static inline bool is_base64(unsigned char c) {
    return (isalnum(c) || (c == '+') || (c == '/'));
}

std::string base64_decode(std::string const& encoded_string) {
    int in_len = encoded_string.size();
    int i = 0;
    int j = 0;
    int in_ = 0;
    unsigned char char_array_4[4], char_array_3[3];
    std::string ret;

    while (in_len-- && (encoded_string[in_] != '=') && is_base64(encoded_string[in_])) {
        char_array_4[i++] = encoded_string[in_]; in_++;
        if (i == 4) {
            for (i = 0; i < 4; i++)
                char_array_4[i] = base64_chars.find(char_array_4[i]);

            char_array_3[0] = (char_array_4[0] << 2) + ((char_array_4[1] & 0x30) >> 4);
            char_array_3[1] = ((char_array_4[1] & 0xf) << 4) + ((char_array_4[2] & 0x3c) >> 2);
            char_array_3[2] = ((char_array_4[2] & 0x3) << 6) + char_array_4[3];

            for (i = 0; (i < 3); i++)
                ret += char_array_3[i];
            i = 0;
        }
    }

    if (i) {
        for (j = i; j < 4; j++)
            char_array_4[j] = 0;

        for (j = 0; j < 4; j++)
            char_array_4[j] = base64_chars.find(char_array_4[j]);

        char_array_3[0] = (char_array_4[0] << 2) + ((char_array_4[1] & 0x30) >> 4);
        char_array_3[1] = ((char_array_4[1] & 0xf) << 4) + ((char_array_4[2] & 0x3c) >> 2);
        char_array_3[2] = ((char_array_4[2] & 0x3) << 6) + char_array_4[3];

        for (j = 0; (j < i - 1); j++) ret += char_array_3[j];
    }

    return ret;
}

// Simple JSON field extractor (minimal parser for our specific needs)
std::string extract_json_field(const std::string& json, const std::string& field_name) {
    std::string search_pattern = "\"" + field_name + "\":";
    size_t start_pos = json.find(search_pattern);
    
    if (start_pos == std::string::npos) {
        return "";
    }
    
    start_pos += search_pattern.length();
    
    // Skip whitespace
    while (start_pos < json.length() && (json[start_pos] == ' ' || json[start_pos] == '\n' || json[start_pos] == '\t')) {
        start_pos++;
    }
    
    // Handle string values (enclosed in quotes)
    if (json[start_pos] == '"') {
        start_pos++; // skip opening quote
        size_t end_pos = start_pos;
        bool escaped = false;
        
        while (end_pos < json.length()) {
            if (escaped) {
                escaped = false;
                end_pos++;
                continue;
            }
            if (json[end_pos] == '\\') {
                escaped = true;
                end_pos++;
                continue;
            }
            if (json[end_pos] == '"') {
                return json.substr(start_pos, end_pos - start_pos);
            }
            end_pos++;
        }
    }
    
    // Handle object values (enclosed in braces)
    if (json[start_pos] == '{') {
        size_t end_pos = start_pos + 1;
        int brace_count = 1;
        bool in_string = false;
        bool escaped = false;
        
        while (end_pos < json.length() && brace_count > 0) {
            if (escaped) {
                escaped = false;
                end_pos++;
                continue;
            }
            
            if (json[end_pos] == '\\' && in_string) {
                escaped = true;
                end_pos++;
                continue;
            }
            
            if (json[end_pos] == '"') {
                in_string = !in_string;
            }
            
            if (!in_string) {
                if (json[end_pos] == '{') brace_count++;
                if (json[end_pos] == '}') brace_count--;
            }
            
            end_pos++;
        }
        
        return json.substr(start_pos, end_pos - start_pos);
    }
    
    return "";
}

//' Fetch model specification from GitHub repository (internal C++ function)
//' 
//' @param api_url Full GitHub API URL to the coefficients.json file
//' @param token GitHub personal access token
//' @return Named list with coefficients_json, prediction_function, metadata_json, and http_status
//' @keywords internal
// [[Rcpp::export(.fetch_github_model_cpp)]]
List fetch_github_model_cpp(std::string api_url, std::string token) {
    CURL *curl;
    CURLcode res;
    std::string response_string;
    long http_code = 0;
    
    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    
    if (!curl) {
        return List::create(
            Named("error") = "Failed to initialize libcurl",
            Named("http_status") = 0
        );
    }
    
    // Set up headers
    struct curl_slist *headers = NULL;
    std::string auth_header = "Authorization: token " + token;
    std::string accept_header = "Accept: application/vnd.github.v3+json";
    
    headers = curl_slist_append(headers, auth_header.c_str());
    headers = curl_slist_append(headers, accept_header.c_str());
    headers = curl_slist_append(headers, "User-Agent: evaluatr-r-package");
    
    // Configure curl
    curl_easy_setopt(curl, CURLOPT_URL, api_url.c_str());
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_string);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 1L);
    
    // Perform the request
    res = curl_easy_perform(curl);
    
    // Get HTTP status code
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);
    
    // Cleanup curl
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    curl_global_cleanup();
    
    // Check for curl errors
    if (res != CURLE_OK) {
        return List::create(
            Named("error") = std::string("CURL error: ") + curl_easy_strerror(res),
            Named("http_status") = (int)http_code
        );
    }
    
    // Check HTTP status
    if (http_code != 200) {
        std::string error_msg;
        if (http_code == 404) {
            error_msg = "Model not found (404). Check repo_owner, repo_name, and model_id.";
        } else if (http_code == 401) {
            error_msg = "Authentication failed (401). Check your GitHub token.";
        } else {
            error_msg = "HTTP error: " + std::to_string(http_code);
        }
        return List::create(
            Named("error") = error_msg,
            Named("http_status") = (int)http_code
        );
    }
    
    // Extract the "content" field (Base64-encoded file content)
    std::string encoded_content = extract_json_field(response_string, "content");
    if (encoded_content.empty()) {
        return List::create(
            Named("error") = "Failed to extract 'content' field from GitHub API response",
            Named("http_status") = (int)http_code
        );
    }
    
    // Remove newlines from Base64 string (GitHub includes line breaks)
    encoded_content.erase(std::remove(encoded_content.begin(), encoded_content.end(), '\n'), encoded_content.end());
    encoded_content.erase(std::remove(encoded_content.begin(), encoded_content.end(), '\r'), encoded_content.end());
    
    // Decode Base64
    std::string decoded_json = base64_decode(encoded_content);
    
    // Extract the three key fields we need
    std::string coefficients_json = extract_json_field(decoded_json, "coefficients");
    std::string prediction_function = extract_json_field(decoded_json, "prediction_function");
    std::string metadata_json = extract_json_field(decoded_json, "metadata");
    
    // Check that we got all required fields
    if (coefficients_json.empty() || prediction_function.empty() || metadata_json.empty()) {
        return List::create(
            Named("error") = "Missing required fields in model JSON (coefficients, prediction_function, or metadata)",
            Named("http_status") = (int)http_code
        );
    }
    
    // Return as R list
    return List::create(
        Named("coefficients_json") = coefficients_json,
        Named("prediction_function") = prediction_function,
        Named("metadata_json") = metadata_json,
        Named("http_status") = (int)http_code,
        Named("success") = true
    );
}
