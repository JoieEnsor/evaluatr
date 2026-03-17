// predict_secure.cpp — C++ Secure Prediction Engine for evaluatr
//
// Depends ONLY on Rcpp. No libcurl, no external C libraries.
// This file implements the full decode → de-obfuscate → predict → shuffle → wipe
// pipeline so that model coefficients never exist as readable R objects.

// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <string>
#include <vector>
#include <map>
#include <cstring>   // memset
#include <cstdint>
#include <algorithm> // std::sort
#include <cmath>     // std::exp, std::log

using namespace Rcpp;

// ============================================================
// INTERNAL SALT — baked into the compiled binary.
// Neither salt alone nor key alone is sufficient to de-obfuscate.
// ============================================================
static const uint64_t SALT_A = UINT64_C(0x9e3779b97f4a7c15);
static const uint64_t SALT_B = UINT64_C(0x6c62272e07bb0142);

// ============================================================
// BASE64 DECODER (pure C++)
// Handles the padded base64 produced by the GitHub Contents API.
// ============================================================

static const int8_t B64_TABLE[256] = {
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,63,
  52,53,54,55,56,57,58,59,60,61,-1,-1,-1, 0,-1,-1,
  -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,
  15,16,17,18,19,20,21,22,23,24,25,-1,-1,-1,-1,-1,
  -1,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
  41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
};

static std::string base64_decode(const std::string& encoded) {
  // Strip whitespace (GitHub API wraps base64 at 60 chars with newlines)
  std::string clean;
  clean.reserve(encoded.size());
  for (char c : encoded) {
    if (c != '\n' && c != '\r' && c != ' ') clean.push_back(c);
  }

  std::string result;
  result.reserve((clean.size() / 4) * 3 + 3);

  size_t i = 0;
  while (i + 3 < clean.size()) {
    int8_t v0 = B64_TABLE[(unsigned char)clean[i]];
    int8_t v1 = B64_TABLE[(unsigned char)clean[i+1]];
    int8_t v2 = B64_TABLE[(unsigned char)clean[i+2]];
    int8_t v3 = B64_TABLE[(unsigned char)clean[i+3]];
    if (v0 < 0 || v1 < 0) break;
    result.push_back((char)((v0 << 2) | (v1 >> 4)));
    if (clean[i+2] != '=') result.push_back((char)(((v1 & 0xF) << 4) | (v2 >> 2)));
    if (clean[i+3] != '=') result.push_back((char)(((v2 & 0x3) << 6) | v3));
    i += 4;
  }
  return result;
}

// ============================================================
// MINIMAL JSON PARSER
// Purpose-built for our schema. Not a general-purpose parser.
// Handles: string values, number values, null, nested objects,
// array of numbers, and the exact nesting used in our JSON spec.
// ============================================================

// Skip whitespace
static size_t skip_ws(const std::string& s, size_t pos) {
  while (pos < s.size() && (s[pos] == ' ' || s[pos] == '\t' ||
         s[pos] == '\n' || s[pos] == '\r')) pos++;
  return pos;
}

// Parse a quoted JSON string, return value and advance pos past closing "
static std::string parse_string(const std::string& s, size_t& pos) {
  // pos must be at opening "
  pos++; // skip "
  std::string result;
  while (pos < s.size() && s[pos] != '"') {
    if (s[pos] == '\\') {
      pos++;
      if (pos < s.size()) {
        char esc = s[pos];
        if      (esc == '"')  result.push_back('"');
        else if (esc == '\\') result.push_back('\\');
        else if (esc == '/')  result.push_back('/');
        else if (esc == 'n')  result.push_back('\n');
        else if (esc == 'r')  result.push_back('\r');
        else if (esc == 't')  result.push_back('\t');
        else                  result.push_back(esc);
        pos++;
      }
    } else {
      result.push_back(s[pos++]);
    }
  }
  if (pos < s.size()) pos++; // skip closing "
  return result;
}

// Parse a JSON number (double), advance pos past number
static double parse_number(const std::string& s, size_t& pos) {
  size_t start = pos;
  if (pos < s.size() && (s[pos] == '-' || s[pos] == '+')) pos++;
  while (pos < s.size() && (std::isdigit(s[pos]) || s[pos] == '.' ||
         s[pos] == 'e' || s[pos] == 'E' || s[pos] == '+' || s[pos] == '-')) pos++;
  return std::stod(s.substr(start, pos - start));
}

// Check if current token is null, advance past it
static bool parse_null(const std::string& s, size_t& pos) {
  if (pos + 3 < s.size() && s.substr(pos, 4) == "null") {
    pos += 4;
    return true;
  }
  return false;
}

// Forward declare
static void skip_value(const std::string& s, size_t& pos);

// Skip an array
static void skip_array(const std::string& s, size_t& pos) {
  pos++; // skip [
  pos = skip_ws(s, pos);
  if (pos < s.size() && s[pos] == ']') { pos++; return; }
  while (pos < s.size()) {
    skip_value(s, pos);
    pos = skip_ws(s, pos);
    if (pos < s.size() && s[pos] == ',') { pos++; pos = skip_ws(s, pos); }
    else if (pos < s.size() && s[pos] == ']') { pos++; return; }
    else break;
  }
}

// Skip an object
static void skip_object(const std::string& s, size_t& pos) {
  pos++; // skip {
  pos = skip_ws(s, pos);
  if (pos < s.size() && s[pos] == '}') { pos++; return; }
  while (pos < s.size()) {
    if (s[pos] == '"') parse_string(s, pos);
    pos = skip_ws(s, pos);
    if (pos < s.size() && s[pos] == ':') { pos++; pos = skip_ws(s, pos); }
    skip_value(s, pos);
    pos = skip_ws(s, pos);
    if (pos < s.size() && s[pos] == ',') { pos++; pos = skip_ws(s, pos); }
    else if (pos < s.size() && s[pos] == '}') { pos++; return; }
    else break;
  }
}

// Skip any JSON value
static void skip_value(const std::string& s, size_t& pos) {
  pos = skip_ws(s, pos);
  if (pos >= s.size()) return;
  char c = s[pos];
  if (c == '"') { parse_string(s, pos); }
  else if (c == '{') { skip_object(s, pos); }
  else if (c == '[') { skip_array(s, pos); }
  else if (c == 'n' || c == 't' || c == 'f') {
    while (pos < s.size() && s[pos] != ',' && s[pos] != '}' &&
           s[pos] != ']' && s[pos] != '\n') pos++;
  } else {
    parse_number(s, pos);
  }
}

// Parse flat coefficient object: {"name1": val1, "name2": val2, ...}
// Returns names and values in parallel vectors
static void parse_flat_coeffs(const std::string& s, size_t& pos,
                               std::vector<std::string>& names,
                               std::vector<double>& values) {
  pos = skip_ws(s, pos);
  if (pos >= s.size() || s[pos] != '{') return;
  pos++; // skip {
  pos = skip_ws(s, pos);
  while (pos < s.size() && s[pos] != '}') {
    if (s[pos] == '"') {
      std::string key = parse_string(s, pos);
      pos = skip_ws(s, pos);
      if (pos < s.size() && s[pos] == ':') { pos++; pos = skip_ws(s, pos); }
      double val = parse_number(s, pos);
      names.push_back(key);
      values.push_back(val);
    }
    pos = skip_ws(s, pos);
    if (pos < s.size() && s[pos] == ',') { pos++; pos = skip_ws(s, pos); }
  }
  if (pos < s.size() && s[pos] == '}') pos++;
}

// Parse array of numbers: [1.0, 2.0, ...]
static std::vector<double> parse_number_array(const std::string& s, size_t& pos) {
  std::vector<double> result;
  pos = skip_ws(s, pos);
  if (pos >= s.size() || s[pos] != '[') return result;
  pos++; pos = skip_ws(s, pos);
  while (pos < s.size() && s[pos] != ']') {
    result.push_back(parse_number(s, pos));
    pos = skip_ws(s, pos);
    if (pos < s.size() && s[pos] == ',') { pos++; pos = skip_ws(s, pos); }
  }
  if (pos < s.size() && s[pos] == ']') pos++;
  return result;
}

// ============================================================
// MODEL SPEC — parsed from JSON
// ============================================================
struct CoeffCategory {
  std::string category_name;
  std::vector<std::string> names;
  std::vector<double> values; // obfuscated or real depending on parse stage
};

struct ModelSpec {
  std::string model_type;
  std::string obfuscation_key;
  bool has_obfuscation_key = false;

  // Standard models: flat coefficients
  std::vector<std::string> coeff_names;
  std::vector<double>      coeff_values;

  // Multinomial: per-category coefficients
  std::vector<CoeffCategory> categories;
  bool is_multinomial = false;

  // Preprocessing
  std::string preprocessing; // may be empty
  bool has_preprocessing = false;

  // model_parameters
  std::vector<double> timepoints;
  std::vector<double> baseline_survival; // Cox
  double weibull_shape = 1.0;
  std::string weibull_parameterisation; // "aft" or "ph"

  // metadata
  std::string model_name;
  std::string version;
  std::vector<std::string> variables; // predictor variable names
  std::string outcome_type;
  std::string description;
};

// Parse metadata object (advances pos past closing '}')
static void parse_metadata(const std::string& json, size_t& pos, ModelSpec& spec) {
  if (pos >= json.size() || json[pos] != '{') return;
  pos++; // skip {
  pos = skip_ws(json, pos);

  while (pos < json.size() && json[pos] != '}') {
    if (json[pos] != '"') break;
    std::string key = parse_string(json, pos);
    pos = skip_ws(json, pos);
    if (pos < json.size() && json[pos] == ':') { pos++; pos = skip_ws(json, pos); }

    if (key == "model_name") {
      if (pos < json.size() && json[pos] == '"') spec.model_name = parse_string(json, pos);
      else skip_value(json, pos);
    } else if (key == "version") {
      if (pos < json.size() && json[pos] == '"') spec.version = parse_string(json, pos);
      else skip_value(json, pos);
    } else if (key == "description") {
      if (pos < json.size() && json[pos] == '"') spec.description = parse_string(json, pos);
      else skip_value(json, pos);
    } else if (key == "outcome_type") {
      if (pos < json.size() && json[pos] == '"') spec.outcome_type = parse_string(json, pos);
      else skip_value(json, pos);
    } else if (key == "variables") {
      if (pos < json.size() && json[pos] == '[') {
        pos++; pos = skip_ws(json, pos);
        while (pos < json.size() && json[pos] != ']') {
          if (json[pos] == '"') spec.variables.push_back(parse_string(json, pos));
          else skip_value(json, pos);
          pos = skip_ws(json, pos);
          if (pos < json.size() && json[pos] == ',') { pos++; pos = skip_ws(json, pos); }
        }
        if (pos < json.size() && json[pos] == ']') pos++;
      } else skip_value(json, pos);
    } else {
      skip_value(json, pos);
    }

    pos = skip_ws(json, pos);
    if (pos < json.size() && json[pos] == ',') { pos++; pos = skip_ws(json, pos); }
  }
  if (pos < json.size() && json[pos] == '}') pos++;
}

// Parse model_parameters object for Cox/Weibull (advances pos past closing '}')
static void parse_model_parameters(const std::string& json, size_t& pos, ModelSpec& spec) {
  if (pos >= json.size() || json[pos] != '{') return;
  pos++;
  pos = skip_ws(json, pos);

  while (pos < json.size() && json[pos] != '}') {
    if (json[pos] != '"') break;
    std::string key = parse_string(json, pos);
    pos = skip_ws(json, pos);
    if (pos < json.size() && json[pos] == ':') { pos++; pos = skip_ws(json, pos); }

    if (key == "timepoints") {
      spec.timepoints = parse_number_array(json, pos);
    } else if (key == "baseline_survival") {
      spec.baseline_survival = parse_number_array(json, pos);
    } else if (key == "shape") {
      spec.weibull_shape = parse_number(json, pos);
    } else if (key == "parameterisation") {
      if (pos < json.size() && json[pos] == '"') spec.weibull_parameterisation = parse_string(json, pos);
      else skip_value(json, pos);
    } else {
      skip_value(json, pos);
    }

    pos = skip_ws(json, pos);
    if (pos < json.size() && json[pos] == ',') { pos++; pos = skip_ws(json, pos); }
  }
  if (pos < json.size() && json[pos] == '}') pos++;
}

// Parse the full JSON into a ModelSpec
static ModelSpec parse_model_json(const std::string& json) {
  ModelSpec spec;
  size_t pos = 0;
  pos = skip_ws(json, pos);
  if (pos >= json.size() || json[pos] != '{') {
    Rcpp::stop("Invalid model JSON: expected top-level object");
  }
  pos++; // skip {
  pos = skip_ws(json, pos);

  while (pos < json.size() && json[pos] != '}') {
    if (json[pos] != '"') break;
    std::string key = parse_string(json, pos);
    pos = skip_ws(json, pos);
    if (pos < json.size() && json[pos] == ':') { pos++; pos = skip_ws(json, pos); }

    if (key == "model_type") {
      if (pos < json.size() && json[pos] == '"') spec.model_type = parse_string(json, pos);
      else skip_value(json, pos);
    } else if (key == "obfuscation_key") {
      if (pos < json.size() && json[pos] == '"') {
        spec.obfuscation_key = parse_string(json, pos);
        spec.has_obfuscation_key = true;
      } else {
        parse_null(json, pos);
        spec.has_obfuscation_key = false;
      }
    } else if (key == "preprocessing") {
      if (pos < json.size() && json[pos] == '"') {
        spec.preprocessing = parse_string(json, pos);
        spec.has_preprocessing = true;
      } else {
        parse_null(json, pos);
        spec.has_preprocessing = false;
      }
    } else if (key == "coefficients") {
      pos = skip_ws(json, pos);
      if (pos >= json.size()) break;
      if (json[pos] == '{') {
        // Peek inside to check if multinomial (values are objects) or flat (values are numbers)
        size_t peek = pos + 1;
        peek = skip_ws(json, peek);
        if (peek < json.size() && json[peek] == '"') {
          // parse key
          size_t key_end = peek;
          parse_string(json, key_end);
          key_end = skip_ws(json, key_end);
          if (key_end < json.size() && json[key_end] == ':') {
            key_end++;
            key_end = skip_ws(json, key_end);
            if (key_end < json.size() && json[key_end] == '{') {
              // Multinomial: nested objects
              spec.is_multinomial = true;
              pos++; // skip outer {
              pos = skip_ws(json, pos);
              while (pos < json.size() && json[pos] != '}') {
                if (json[pos] != '"') break;
                CoeffCategory cat;
                cat.category_name = parse_string(json, pos);
                pos = skip_ws(json, pos);
                if (pos < json.size() && json[pos] == ':') { pos++; pos = skip_ws(json, pos); }
                parse_flat_coeffs(json, pos, cat.names, cat.values);
                spec.categories.push_back(cat);
                pos = skip_ws(json, pos);
                if (pos < json.size() && json[pos] == ',') { pos++; pos = skip_ws(json, pos); }
              }
              if (pos < json.size() && json[pos] == '}') pos++;
            } else {
              // Flat coefficients
              parse_flat_coeffs(json, pos, spec.coeff_names, spec.coeff_values);
            }
          } else {
            skip_value(json, pos);
          }
        } else if (peek < json.size() && json[peek] == '}') {
          // empty object
          pos++; pos++;
        } else {
          skip_value(json, pos);
        }
      } else {
        skip_value(json, pos);
      }
    } else if (key == "model_parameters") {
      pos = skip_ws(json, pos);
      if (pos < json.size() && json[pos] == '{') {
        // parse_model_parameters parses AND advances pos past the closing '}'
        parse_model_parameters(json, pos, spec);
      } else {
        skip_value(json, pos);
      }
    } else if (key == "metadata") {
      pos = skip_ws(json, pos);
      if (pos < json.size() && json[pos] == '{') {
        // parse_metadata parses AND advances pos past the closing '}'
        parse_metadata(json, pos, spec);
      } else {
        skip_value(json, pos);
      }
    } else {
      skip_value(json, pos);
    }

    pos = skip_ws(json, pos);
    if (pos < json.size() && json[pos] == ',') { pos++; pos = skip_ws(json, pos); }
  }
  return spec;
}

// ============================================================
// SPLITMIX64 PRNG
// ============================================================
static uint64_t splitmix64(uint64_t& state) {
  state += UINT64_C(0x9e3779b97f4a7c15);
  uint64_t z = state;
  z = (z ^ (z >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
  z = (z ^ (z >> 27)) * UINT64_C(0x94d049bb133111eb);
  return z ^ (z >> 31);
}

// Seed PRNG from obfuscation_key hex string + compiled salts
static uint64_t seed_from_key(const std::string& key) {
  uint64_t h = SALT_A;
  for (char c : key) {
    h ^= (uint64_t)(unsigned char)c;
    h = (h << 13) | (h >> 51);
    h *= SALT_B;
    h ^= h >> 33;
  }
  return h;
}

// Convert uint64 to double in [0, 1)
static double u64_to_double(uint64_t v) {
  return (double)(v >> 11) * (1.0 / (double)(UINT64_C(1) << 53));
}

// ============================================================
// OBFUSCATION / DE-OBFUSCATION
// ============================================================

// Generate per-coefficient transform parameters given index and PRNG state
static void get_transform_params(uint64_t& state, double& mult, double& offset) {
  // Multiplier in [0.3, 3.0], 30% chance of sign flip
  double raw_mult = 0.3 + u64_to_double(splitmix64(state)) * 2.7;
  uint64_t flip_rnd = splitmix64(state);
  bool flip = (flip_rnd & 0xF) < 5; // ~31% chance
  mult = flip ? -raw_mult : raw_mult;
  // Offset in [-50, 50]
  offset = -50.0 + u64_to_double(splitmix64(state)) * 100.0;
}

// De-obfuscate: real = (stored - offset) / mult
static void deobfuscate_coeffs(std::vector<double>& values,
                                const std::string& obfuscation_key) {
  uint64_t state = seed_from_key(obfuscation_key);
  for (size_t i = 0; i < values.size(); i++) {
    double mult, offset;
    get_transform_params(state, mult, offset);
    values[i] = (values[i] - offset) / mult;
  }
}

// Obfuscate: stored = (real * mult) + offset
static void obfuscate_coeffs(std::vector<double>& values,
                              const std::string& obfuscation_key) {
  uint64_t state = seed_from_key(obfuscation_key);
  for (size_t i = 0; i < values.size(); i++) {
    double mult, offset;
    get_transform_params(state, mult, offset);
    values[i] = (values[i] * mult) + offset;
  }
}

// ============================================================
// PREDICTION FUNCTIONS
// ============================================================

// --- Logistic regression ---
static std::vector<double> predict_logistic(
    const std::vector<std::string>& coeff_names,
    const std::vector<double>& coeff_values,
    const NumericMatrix& X) {

  int n = X.nrow();
  int p = X.ncol();
  std::vector<double> preds(n, 0.0);

  for (int i = 0; i < n; i++) {
    double lp = 0.0;
    // coeff_values and X columns must align (caller ensures this)
    for (int j = 0; j < p && j < (int)coeff_values.size(); j++) {
      lp += coeff_values[j] * X(i, j);
    }
    preds[i] = 1.0 / (1.0 + std::exp(-lp));
  }
  return preds;
}

// --- Cox proportional hazards ---
// Prediction: survival probability at each timepoint
// S(t) = S0(t)^exp(LP)
static NumericMatrix predict_cox(
    const std::vector<std::string>& coeff_names,
    const std::vector<double>& coeff_values,
    const NumericMatrix& X,
    const std::vector<double>& timepoints,
    const std::vector<double>& baseline_survival) {

  int n = X.nrow();
  int p = X.ncol();
  int nt = (int)timepoints.size();

  NumericMatrix result(n, nt);
  CharacterVector col_names(nt);
  for (int t = 0; t < nt; t++) {
    col_names[t] = "t" + std::to_string((int)timepoints[t]);
  }
  colnames(result) = col_names;

  for (int i = 0; i < n; i++) {
    double lp = 0.0;
    for (int j = 0; j < p && j < (int)coeff_values.size(); j++) {
      lp += coeff_values[j] * X(i, j);
    }
    double exp_lp = std::exp(lp);
    for (int t = 0; t < nt; t++) {
      double s0 = baseline_survival[t];
      if (s0 <= 0.0) s0 = 1e-10;
      if (s0 > 1.0) s0 = 1.0 - 1e-10;
      result(i, t) = std::pow(s0, exp_lp);
    }
  }
  return result;
}

// --- Weibull ---
// AFT: S(t) = exp(-( t / exp(mu + X*beta) )^shape )
// PH:  S(t) = exp(-lambda * t^shape * exp(X*beta))
//      where lambda = exp(intercept), shape from model_parameters
static NumericMatrix predict_weibull(
    const std::vector<std::string>& coeff_names,
    const std::vector<double>& coeff_values,
    const NumericMatrix& X,
    const std::vector<double>& timepoints,
    double shape,
    const std::string& parameterisation) {

  int n = X.nrow();
  int p = X.ncol();
  int nt = (int)timepoints.size();

  NumericMatrix result(n, nt);
  CharacterVector col_names(nt);
  for (int t = 0; t < nt; t++) {
    col_names[t] = "t" + std::to_string((int)timepoints[t]);
  }
  colnames(result) = col_names;

  bool is_aft = (parameterisation != "ph");

  for (int i = 0; i < n; i++) {
    double lp = 0.0;
    for (int j = 0; j < p && j < (int)coeff_values.size(); j++) {
      lp += coeff_values[j] * X(i, j);
    }

    for (int t = 0; t < nt; t++) {
      double tv = timepoints[t];
      double surv;
      if (is_aft) {
        // scale = exp(lp), S(t) = exp(-(t/scale)^shape)
        double scale = std::exp(lp);
        if (scale <= 0) scale = 1e-10;
        surv = std::exp(-std::pow(tv / scale, shape));
      } else {
        // PH: intercept = log(lambda), rest are covariates
        // lp here includes intercept so lambda*exp(X*b) = exp(lp)
        surv = std::exp(-std::exp(lp) * std::pow(tv, shape));
      }
      result(i, t) = surv;
    }
  }
  return result;
}

// --- Multinomial logistic (softmax) ---
// categories is a vector of CoeffCategory, each with names+values
// Returns n x k matrix (one column per category, including reference = 1/denom)
static NumericMatrix predict_multinomial(
    const std::vector<CoeffCategory>& categories,
    const NumericMatrix& X,
    int n_total) {

  int n = X.nrow();
  int k = (int)categories.size(); // number of non-reference categories

  // Compute LP for each non-reference category
  std::vector<std::vector<double>> lps(k, std::vector<double>(n, 0.0));

  for (int c = 0; c < k; c++) {
    const CoeffCategory& cat = categories[c];
    int p = X.ncol();
    for (int i = 0; i < n; i++) {
      double lp = 0.0;
      for (int j = 0; j < p && j < (int)cat.values.size(); j++) {
        lp += cat.values[j] * X(i, j);
      }
      lps[c][i] = lp;
    }
  }

  // Softmax: p_ref = 1/denom, p_c = exp(LP_c)/denom
  NumericMatrix result(n, k + 1); // +1 for reference
  CharacterVector col_names(k + 1);
  col_names[0] = "reference";
  for (int c = 0; c < k; c++) col_names[c+1] = categories[c].category_name;
  colnames(result) = col_names;

  for (int i = 0; i < n; i++) {
    double denom = 1.0;
    for (int c = 0; c < k; c++) denom += std::exp(lps[c][i]);
    result(i, 0) = 1.0 / denom; // reference category probability
    for (int c = 0; c < k; c++) result(i, c+1) = std::exp(lps[c][i]) / denom;
  }
  return result;
}

// ============================================================
// WIPE SENSITIVE DATA
// ============================================================
static void wipe_spec(ModelSpec& spec) {
  // Overwrite coefficient values with zeros before clearing
  for (size_t i = 0; i < spec.coeff_values.size(); i++) spec.coeff_values[i] = 0.0;
  spec.coeff_values.clear();
  spec.coeff_names.clear();
  for (auto& cat : spec.categories) {
    for (size_t i = 0; i < cat.values.size(); i++) cat.values[i] = 0.0;
    cat.values.clear();
    cat.names.clear();
  }
  spec.categories.clear();
  spec.obfuscation_key = std::string(spec.obfuscation_key.size(), '\0');
}

// ============================================================
// EXPORTED FUNCTIONS
// ============================================================

//' Extract model metadata from base64-encoded JSON content
//'
//' @description
//' Decodes the base64 GitHub API content string and returns only metadata
//' (model type, variable names, preprocessing code). Does NOT return
//' coefficient values.
//'
//' @param encoded_content Character. Base64-encoded JSON string from GitHub
//'   Contents API.
//' @return A named R list containing model_type, variable_names,
//'   model_name, version, preprocessing, is_multinomial,
//'   category_names, has_obfuscation_key, model_parameters_present.
//'
//' @keywords internal
// [[Rcpp::export(.extract_model_metadata_cpp)]]
List extract_model_metadata_cpp(std::string encoded_content) {
  std::string json = base64_decode(encoded_content);
  if (json.empty()) Rcpp::stop("Failed to decode base64 content");

  ModelSpec spec = parse_model_json(json);

  // Collect variable names (never return coefficient values)
  CharacterVector var_names;
  if (!spec.variables.empty()) {
    var_names = wrap(spec.variables);
  } else if (spec.is_multinomial && !spec.categories.empty()) {
    // Derive variable names from first category's coeff names minus intercept
    std::vector<std::string> vn;
    for (const auto& n : spec.categories[0].names) {
      if (n != "(Intercept)") vn.push_back(n);
    }
    var_names = wrap(vn);
  } else {
    std::vector<std::string> vn;
    for (const auto& n : spec.coeff_names) {
      if (n != "(Intercept)") vn.push_back(n);
    }
    var_names = wrap(vn);
  }

  // Category names for multinomial
  CharacterVector cat_names;
  if (spec.is_multinomial) {
    std::vector<std::string> cn;
    for (const auto& cat : spec.categories) cn.push_back(cat.category_name);
    cat_names = wrap(cn);
  }

  // Wipe coefficient data before returning
  wipe_spec(spec);

  return List::create(
    Named("model_type")              = spec.model_type,
    Named("variable_names")          = var_names,
    Named("model_name")              = spec.model_name,
    Named("version")                 = spec.version,
    Named("outcome_type")            = spec.outcome_type,
    Named("preprocessing")           = spec.preprocessing,
    Named("has_preprocessing")       = spec.has_preprocessing,
    Named("is_multinomial")          = spec.is_multinomial,
    Named("category_names")          = cat_names,
    Named("has_obfuscation_key")     = spec.has_obfuscation_key,
    Named("description")             = spec.description,
    Named("model_parameters_present") = (!spec.timepoints.empty())
  );
}


//' Full secure prediction pipeline
//'
//' @description
//' Decodes base64 content, de-obfuscates coefficients, computes predictions,
//' shuffles outcome-prediction pairs, wipes coefficient data, and returns only
//' the shuffled results. Coefficients never exist as readable R objects.
//'
//' @param encoded_content Character. Base64-encoded JSON from GitHub API.
//' @param model_type Character. Model type string from metadata.
//' @param design_matrix Numeric matrix. Rows = observations, columns = terms
//'   (intercept first if present, then predictors in order).
//' @param outcome_vec Numeric vector. Outcome variable values.
//' @param by_vec Character/numeric vector (or R NULL). Subgroup variable.
//' @param model_params Named list (or NULL) with additional model parameters
//'   passed from R for reference: timepoints, baseline_survival, etc.
//'   (Actually these are parsed from JSON; the R list is used only for
//'   future extensibility and is currently ignored — parameters come from JSON.)
//' @return A named R list with shuffled_outcomes, shuffled_predictions (logistic)
//'   or prediction_matrix (multi-column), shuffled_by (if by_vec non-null).
//'
//' @keywords internal
// [[Rcpp::export(.predict_from_encoded_cpp)]]
List predict_from_encoded_cpp(std::string encoded_content,
                               std::string model_type,
                               NumericMatrix design_matrix,
                               NumericVector outcome_vec,
                               SEXP by_vec,
                               SEXP model_params) {

  // Decode and parse
  std::string json = base64_decode(encoded_content);
  if (json.empty()) Rcpp::stop("Failed to decode base64 content");

  ModelSpec spec = parse_model_json(json);

  // De-obfuscate if key present
  if (spec.has_obfuscation_key && !spec.obfuscation_key.empty()) {
    if (!spec.is_multinomial) {
      deobfuscate_coeffs(spec.coeff_values, spec.obfuscation_key);
    } else {
      for (auto& cat : spec.categories) {
        deobfuscate_coeffs(cat.values, spec.obfuscation_key);
      }
    }
  }

  int n = design_matrix.nrow();

  // ---- Compute predictions ---------------------------------------------------
  NumericMatrix pred_matrix;
  bool is_single_col = true;

  std::string mtype = spec.model_type.empty() ? model_type : spec.model_type;

  if (mtype == "logistic" || mtype == "logistic_regression" || mtype == "binary") {
    std::vector<double> pv = predict_logistic(spec.coeff_names, spec.coeff_values,
                                              design_matrix);
    pred_matrix = NumericMatrix(n, 1);
    colnames(pred_matrix) = CharacterVector::create("prediction");
    for (int i = 0; i < n; i++) pred_matrix(i, 0) = pv[i];
    is_single_col = true;

  } else if (mtype == "cox") {
    if (spec.timepoints.empty()) {
      // Wipe before error
      wipe_spec(spec);
      Rcpp::stop("Cox model requires timepoints in model_parameters");
    }
    if (spec.baseline_survival.empty()) {
      wipe_spec(spec);
      Rcpp::stop("Cox model requires baseline_survival in model_parameters");
    }
    pred_matrix = predict_cox(spec.coeff_names, spec.coeff_values, design_matrix,
                              spec.timepoints, spec.baseline_survival);
    is_single_col = (pred_matrix.ncol() == 1);

  } else if (mtype == "weibull") {
    if (spec.timepoints.empty()) {
      wipe_spec(spec);
      Rcpp::stop("Weibull model requires timepoints in model_parameters");
    }
    std::string param = spec.weibull_parameterisation.empty() ? "aft" :
                        spec.weibull_parameterisation;
    pred_matrix = predict_weibull(spec.coeff_names, spec.coeff_values, design_matrix,
                                  spec.timepoints, spec.weibull_shape, param);
    is_single_col = (pred_matrix.ncol() == 1);

  } else if (mtype == "multinomial") {
    if (spec.categories.empty()) {
      wipe_spec(spec);
      Rcpp::stop("Multinomial model has no coefficient categories");
    }
    pred_matrix = predict_multinomial(spec.categories, design_matrix, n);
    is_single_col = false;

  } else {
    wipe_spec(spec);
    Rcpp::stop("Unsupported model type: '" + mtype +
               "'. Supported: logistic, cox, weibull, multinomial");
  }

  // ---- Wipe coefficients IMMEDIATELY after predictions computed --------------
  wipe_spec(spec);

  // ---- Fisher-Yates shuffle using R's RNG ------------------------------------
  IntegerVector indices = seq_len(n) - 1; // 0-based
  // Use R's RNG (GetRNGstate / PutRNGstate)
  GetRNGstate();
  for (int i = n - 1; i > 0; i--) {
    int j = (int)(unif_rand() * (i + 1));
    if (j > i) j = i; // guard
    int tmp = indices[i];
    indices[i] = indices[j];
    indices[j] = tmp;
  }
  PutRNGstate();

  // ---- Build result ----------------------------------------------------------
  NumericVector shuffled_outcomes(n);
  for (int i = 0; i < n; i++) shuffled_outcomes[i] = outcome_vec[indices[i]];

  NumericMatrix shuffled_pred(n, pred_matrix.ncol());
  if (pred_matrix.hasAttribute("dimnames")) {
    shuffled_pred.attr("dimnames") = pred_matrix.attr("dimnames");
  }
  for (int i = 0; i < n; i++) {
    for (int c = 0; c < pred_matrix.ncol(); c++) {
      shuffled_pred(i, c) = pred_matrix(indices[i], c);
    }
  }

  List result;
  result["shuffled_outcomes"] = shuffled_outcomes;
  result["is_single_col"]     = is_single_col;
  result["shuffled_pred_matrix"] = shuffled_pred;
  result["shuffle_indices"]   = indices;

  // Handle by_vec
  bool has_by = !Rf_isNull(by_vec);
  result["has_by"] = has_by;
  if (has_by) {
    // by_vec can be character or numeric — pass through as-is
    SEXP by_shuffled;
    PROTECT(by_shuffled = Rf_allocVector(TYPEOF(by_vec), n));
    for (int i = 0; i < n; i++) {
      if (TYPEOF(by_vec) == STRSXP) {
        SET_STRING_ELT(by_shuffled, i, STRING_ELT(by_vec, indices[i]));
      } else {
        REAL(by_shuffled)[i] = REAL(by_vec)[indices[i]];
      }
    }
    // Copy names attribute if present
    if (TYPEOF(by_vec) == STRSXP) {
      // convert to CharacterVector for cleaner handling
    }
    result["shuffled_by"] = by_shuffled;
    UNPROTECT(1);
  }

  return result;
}


//' Obfuscate coefficient values
//'
//' @description
//' Takes real coefficient values and returns obfuscated values using the
//' affine transform: stored = (real * mult) + offset, where mult and offset
//' are deterministically generated from the obfuscation_key plus compiled salts.
//' Used by the developer utility (Phase 2) when creating model JSON files.
//'
//' @param real_values Named numeric vector of real coefficient values.
//' @param obfuscation_key Character. 32-character hex string.
//' @return Named numeric vector of obfuscated values.
//'
//' @keywords internal
// [[Rcpp::export(.obfuscate_coefficients_cpp)]]
NumericVector obfuscate_coefficients_cpp(NumericVector real_values,
                                          std::string obfuscation_key) {
  int n = real_values.size();
  std::vector<double> vals(n);
  for (int i = 0; i < n; i++) vals[i] = real_values[i];

  obfuscate_coeffs(vals, obfuscation_key);

  NumericVector result(n);
  for (int i = 0; i < n; i++) result[i] = vals[i];

  // Preserve names if present
  if (real_values.hasAttribute("names")) {
    result.attr("names") = real_values.attr("names");
  }
  return result;
}
