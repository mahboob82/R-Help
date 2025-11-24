# Install required packages
```r
install.packages(c("gtsummary", "flextable", "officer", "dplyr"))
```

# 1. Using as_flex_table() with flextable (Recommended)

```r
library(gtsummary)
library(flextable)
library(officer)


# Create a gtsummary table

tbl <- trial %>%
  select(age, grade, stage, trt) %>%
  tbl_summary(by = trt) %>%
  add_p() %>%
  add_overall()

# Convert to flextable and save to Word
tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "gtsummary_table.docx")


# 2. Using as_gt() with gt package

library(gtsummary)
library(gt)

tbl <- trial %>%
  select(age, grade, response, trt) %>%
  tbl_summary(by = trt) %>%
  add_p()

# Save using gt's export function
gt_table <- as_gt(tbl)
gtsave(gt_table, filename = "gtsummary_table.docx")


# 5. Complete Workflow Example

library(gtsummary)
library(flextable)
library(officer)
library(dplyr)

# Create a more complex table
tbl <- trial %>%
  select(trt, age, marker, stage, grade, response) %>%
  tbl_summary(
    by = trt,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "Missing"
  ) %>%
  add_p(
    test = list(all_continuous() ~ "t.test",
                all_categorical() ~ "chisq.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("**Table 1. Patient Characteristics by Treatment Group**")

# Save with custom formatting
flex_table <- tbl %>%
  as_flex_table() %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Arial", part = "all") %>%
  set_table_properties(layout = "autofit")

# Save to Word
save_as_docx(flex_table, path = "clinical_table.docx")


# 7. Advanced Formatting Options
library(gtsummary)
library(flextable)

tbl <- trial %>%
  tbl_summary(
    by = trt,
    include = c(age, stage, grade)
  ) %>%
  add_p() %>%
  bold_p(t = 0.05) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Treatment Group**")

# Advanced flextable formatting
flex_table <- tbl %>%
  as_flex_table() %>%
  
  # Table formatting
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "center", part = "header") %>%
  align(align = "left", part = "body") %>%
  padding(padding = 2, part = "all") %>%
  bg(bg = "#f0f0f0", part = "header") %>%
  bold(part = "header") %>%
  
  # Border formatting
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 1)) %>%
  border_inner_v(border = fp_border(color = "gray", width = 1)) %>%
  
  # Auto-fit columns
  set_table_properties(layout = "autofit", width = 1)

# Save with specific path
save_as_docx(flex_table, path = "formatted_table.docx")



# If tables are too wide:

tbl %>%
  as_flex_table() %>%
  fontsize(size = 8) %>%  # Smaller font
  set_table_properties(layout = "autofit", width = 0.9) %>%  # Fit to page
  fit_to_width(max_width = 7)  # Maximum width in inches

# If you need landscape orientation:

library(officer)

# Create landscape document
landscape_doc <- read_docx() %>%
  body_end_section_landscape() %>%
  body_add_flextable(as_flex_table(tbl))

print(landscape_doc, target = "landscape_table.docx")
