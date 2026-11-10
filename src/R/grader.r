library(dplyr)


setwd("C://Users/dbarthur/Downloads/submissions")

key <- '
{
  "Question 1": "System 2 is more reliable. System 1 has successfully produces 
  an output with probability 0.6 whereas system 2 successfully produces an output
  with probability 0.72",
  "Question 2": "In system 1, probabilities of functioning are 0.888, 0.8018, and
  0.8427 respectively. In system 2, probabilities of functioning are 0.89, 0.6876,
  0.7416, and 0.8738. These are obtaining by computing conditional probabilities of
  a component functioning given that the previous components functioned. This gives
  an accurate view that the individual component will function. Given this, component B
  in system 1 should be fixed and component B in system 2 should be fixed as well.",
  "Question 3": "System 1 is more likely. There is a 0.5556 chance it is system 1. This
  is obtained using Bayes Theorem 0.6*0.6/(0.6*0.6 + 0.72*0.4).",
  "Question 4": "By the strict definition of independence, no. None of the products of
  marginals will be exactly equalt to the joint. However, if you think about the systems,
  it is clear that the components in system 1 cannot be independent since each subsequent
  one dependends on the previous one. In system 2, in theory, components B and C are
  independent. Knowing that B functioned does not tell me anything about C functioning
  and vice versa. However, it is fine if you applied the strict definition and said
  none were independent.",
  "Question 5": "This is equal to the one minus the probability of all components
  functioning. In this case that would be 1-(0.888*0.8018*0.8427) which is about 0.4"
}
'

assignments <- list.files()
assignments <- assignments[endsWith(assignments, ".html")]

grade_assignment <- function(assignment, key, first_name, last_name){
  student_answers <- rvest::read_html(assignment) %>% rvest::html_text2()
  student_answers <- stringr::str_remove_all(student_answers, first_name)
  student_answers <- stringr::str_remove_all(student_answers, last_name)
  
  prompt <- sprintf('You are an expert grader that is going to grade an assignment
  on using probabilities to analyze two systems. You will be given a key that explains
  an answer to each question. Your job is to use that key to determine a grade for
  each response to the five questions. For any questions that students got wrong,
  return the grade using the following format:
  
  "question_number": {
    "grade": number for grade,
    "reason_for_grade": brief explanation of reason for grade
  }
  
  If they do not miss anything, return an empty JSON object. Use the following
  grading guide for each question:
  
  5 - Gold standard, their answer matches the given answer.
  4 - Incorrect answer, but they had the right idea and employed the right techniques.
  3 - Incorrect answer and methods, but they put the effort into defending their answer.
  2 - Incorrect answer, method, and weak defense/reasoning of answer.
  1 - No answer or completely wrong answer.
  
  Be lenient. Most students should get at least a 3 unless the answer is clearly wrong.
  
  Here is the assignment: %s and here is the key: %s', student_answers, key)
  
  result <- openai::create_chat_completion(
    model = "gpt-5.4-mini",
    messages = list(
      list(role = "user", content = prompt)
    )
  )
  
  graded_assignment <- jsonlite::fromJSON(result$choices$message.content)
  
  graded_assignment
}

# grade_assignment <- function(assignment, key){
#   
#   target_examples <- c(
#     "Example 9.2",
#     "Example 9.3",
#     "Example 9.6",
#     "Example 9.9",
#     "Example 9.12"
#   )
#   
#   doc <- rvest::read_html(assignment)
#   
#   slides <- doc %>% rvest::html_elements("section.slide")
#   
#   student_answers <- slides %>%
#     purrr::map(function(sec) {
#       
#       label <- sec %>%
#         rvest::html_element("div[title^='Example'] .callout-title strong") %>%
#         rvest::html_text(trim = TRUE)
#       
#       if (is.na(label) || !(label %in% target_examples)) {
#         return(NULL)
#       }
#       
#       full_text <- sec %>%
#         rvest::html_text2() %>%
#         stringr::str_squish()
#       
#       setNames(full_text, label)
#     }) %>%
#     purrr::compact() %>%        # remove NULLs
#     purrr::flatten_chr() %>%        # turn list into named character vector
#     paste0(collapse = " ")
#   
#   prompt <- sprintf('You are an expert grader that is going to grade probability assignments.
#   You will be given a key for the assignment in addition to the assignment itself. For
#   each problem you grade, see if they got it right or wrong. For problems they got incorrect,
#   return a JSON object using the following template for each missed problem:
#   
#   "problem_name": {
#     "reason_for_grade": brief explanation of reason for grade
#   }
#   
#   If they do not miss anything, return an empty JSON object. Grade on the more lenient side.
#   If something was not exactly the same as the answer in the answer key, but it was
#   pretty close, then award full credit. Only award no
#   credit when the answer is clearly wrong. Make sure to explain the grade. Here is the
#   assignment: %s and here is the key: %s', student_answers, key)
#   
#   result <- openai::create_chat_completion(
#     model = "gpt-5.4-mini",
#     messages = list(
#       list(role = "user", content = prompt)
#     )
#   )
#   
#   graded_assignment <- jsonlite::fromJSON(result$choices$message.content)
#   
#   graded_assignment
# }

first_name <- "Ryan"
last_name <- "Nguyen"
grade_assignment(assignments[i], key, first_name, last_name)
assignments[i]
i <- i+1