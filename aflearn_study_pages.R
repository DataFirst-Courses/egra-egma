# Generates self-contained study detail HTML pages (sourced from aflearn_dataset_reference_5.qmd)

AF_COUNTRY_SLUG <- c(
  CD = "drc",
  DJ = "djibouti",
  EG = "egypt",
  ET = "ethiopia",
  GH = "ghana",
  KE = "kenya",
  LR = "liberia",
  MW = "malawi",
  MA = "morocco",
  RW = "rwanda",
  TZ = "tanzania",
  ZM = "zambia"
)

slugify_text <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-|-$)", "", x)
}

format_languages <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || s == "") return("\u2014")
    langs <- trimws(unlist(strsplit(as.character(s), ",", fixed = TRUE)))
    langs <- langs[nzchar(langs)]
    if (!length(langs)) return("\u2014")
    paste(langs, collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
}

AF_ISO_COUNTRY <- c(
  CD = "Democratic Republic of the Congo",
  DJ = "Djibouti",
  EG = "Egypt",
  ET = "Ethiopia",
  GH = "Ghana",
  KE = "Kenya",
  LR = "Liberia",
  MW = "Malawi",
  MA = "Morocco",
  RW = "Rwanda",
  TZ = "Tanzania",
  ZM = "Zambia"
)

country_url_slug <- function(country, iso, source = NA_character_) {
  iso <- ifelse(is.na(iso) | iso == "" | iso == "XX", NA_character_, iso)
  if (!is.na(iso) && iso %in% names(AF_COUNTRY_SLUG)) {
    return(unname(AF_COUNTRY_SLUG[[iso]]))
  }
  if (!is.na(country) && country != "" && !tolower(country) %in% c("na", "—")) {
    return(slugify_text(country))
  }
  src <- tolower(as.character(source))
  if (grepl("zambia", src, fixed = TRUE)) return("zambia")
  if (grepl("djibouti|usaid_001", src, fixed = FALSE)) return("djibouti")
  "unknown"
}

grade_url_suffix <- function(grades) {
  if (is.na(grades) || grades == "—") return("")
  nums <- unlist(regmatches(grades, gregexpr("[0-9]+", grades)))
  if (length(nums) == 0) return("")
  if (length(nums) == 1) return(paste0("-g", nums[1]))
  paste0("-g", paste(nums, collapse = "-"))
}

study_index_url <- function(study_slug) {
  n_up <- length(strsplit(study_slug, "/", fixed = TRUE)[[1]])
  paste0(paste(rep("..", n_up), collapse = "/"), "/index.html")
}

assign_study_url_slugs <- function(df) {
  country_slug <- mapply(
    country_url_slug, df$Country, df$ISO, df$Source,
    USE.NAMES = FALSE
  )
  url_base <- paste0(country_slug, "/", df$Year)
  grade_tag <- vapply(df$Grades, grade_url_suffix, character(1))
  n_per_base <- vapply(url_base, function(b) sum(url_base == b), integer(1))

  study_slug <- ifelse(n_per_base > 1L, paste0(url_base, grade_tag), url_base)

  dup <- duplicated(study_slug) | duplicated(study_slug, fromLast = TRUE)
  if (any(dup)) {
    study_slug[dup] <- paste0(study_slug[dup], "-r", df$Round[dup])
  }

  dup2 <- duplicated(study_slug) | duplicated(study_slug, fromLast = TRUE)
  if (any(dup2)) {
    for (i in which(dup2)) {
      acr <- df$Acronym[i]
      extra <- if (!is.na(acr) && acr != "") {
        paste0("-", slugify_text(acr))
      } else {
        paste0("-", slugify_text(df$Source[i]))
      }
      study_slug[i] <- paste0(study_slug[i], extra)
    }
  }

  df$country_slug <- country_slug
  df$study_slug <- study_slug
  # Crosstalk keys must not contain "/" (breaks filter + reactable linkage).
  df$study_key <- gsub("/", "_", study_slug, fixed = TRUE)
  df$Detail_url <- paste0("studies/", study_slug, ".html")
  df
}

clean_header_names <- function(nms) {
  trimws(gsub("\u00a0", " ", nms, fixed = TRUE))
}

load_subtask_desc <- function(path) {
  raw <- readxl::read_excel(path, sheet = "sub-tasks-desc")
  names(raw) <- clean_header_names(names(raw))
  prefix_col  <- names(raw)[grep("prefix", names(raw), ignore.case = TRUE)][1]
  task_col    <- names(raw)[grep("^task", names(raw), ignore.case = TRUE)][1]
  assess_col  <- names(raw)[grep("assessment", names(raw), ignore.case = TRUE)][1]
  core_col    <- names(raw)[grep("^core", names(raw), ignore.case = TRUE)][1]
  alt_col     <- names(raw)[grep("alternate", names(raw), ignore.case = TRUE)][1]

  rows <- lapply(seq_len(nrow(raw)), function(i) {
    prefix <- normalize_task_key(raw[[prefix_col]][i])
    label  <- trimws(as.character(raw[[task_col]][i]))
    if (!nzchar(prefix) || is.na(label) || label == "") return(NULL)
    out <- data.frame(
      task_id = prefix,
      task_label = label,
      assessment = if (!is.na(assess_col)) trimws(as.character(raw[[assess_col]][i])) else "",
      core = if (!is.na(core_col)) trimws(as.character(raw[[core_col]][i])) else "",
      stringsAsFactors = FALSE
    )
    if (!is.na(alt_col)) {
      alts <- strsplit(as.character(raw[[alt_col]][i]), ",")[[1]]
      alts <- normalize_task_key(trimws(alts))
      alts <- alts[nzchar(alts)]
      if (length(alts)) {
        out <- rbind(
          out,
          data.frame(
            task_id = alts,
            task_label = label,
            assessment = out$assessment[1],
            core = out$core[1],
            stringsAsFactors = FALSE
          )
        )
      }
    }
    out
  })
  lookup <- do.call(rbind, rows)
  lookup <- lookup[!is.na(lookup$task_id) & nzchar(lookup$task_id), , drop = FALSE]
  lookup[!duplicated(lookup$task_id), , drop = FALSE]
}

load_va_matrix <- function(path) {
  raw <- readxl::read_excel(path, sheet = "va-matrix")
  names(raw) <- clean_header_names(names(raw))
  prefix_col <- names(raw)[grep("prefix", names(raw), ignore.case = TRUE)][1]
  task_col   <- names(raw)[grep("^task", names(raw), ignore.case = TRUE)][1]
  assess_col <- names(raw)[grep("assessment", names(raw), ignore.case = TRUE)][1]
  core_col   <- names(raw)[grep("^core", names(raw), ignore.case = TRUE)][1]
  meta_cols  <- unique(c(assess_col, task_col, core_col, prefix_col,
                         names(raw)[grep("alternate", names(raw), ignore.case = TRUE)]))
  meta_cols  <- meta_cols[!is.na(meta_cols)]
  study_cols <- setdiff(names(raw), meta_cols)

  present_val <- function(x) {
  v <- toupper(trimws(as.character(x)))
    !is.na(v) & v %in% c("YES", "Y", "TRUE", "1")
  }

  pieces <- lapply(study_cols, function(col) {
    data.frame(
      matrix_col = col,
      task_id = normalize_task_key(raw[[prefix_col]]),
      task_label = trimws(as.character(raw[[task_col]])),
      assessment = if (!is.na(assess_col)) trimws(as.character(raw[[assess_col]])) else "",
      core = if (!is.na(core_col)) trimws(as.character(raw[[core_col]])) else "",
      present = present_val(raw[[col]]),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  out <- out[nzchar(out$task_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

assign_matrix_col <- function(df) {
  df$matrix_col <- df$study_key
  liberia <- grepl("^liberia_", df$study_key, fixed = FALSE) | df$Source == "liberia"
  malawi  <- grepl("^malawi_", df$study_key, fixed = FALSE) | df$Source == "malawi"
  df$matrix_col[liberia] <- "liberia"
  df$matrix_col[malawi] <- "malawi"
  ghana <- df$Source == "ghana_2013_g2" |
    df$study_key %in% c("ghana_2013", "ghana_2013-g2")
  df$matrix_col[ghana] <- "ghana_2013_g2"
  df
}

study_present_subtasks <- function(matrix_long, matrix_col, desc_lookup = NULL) {
  if (is.na(matrix_col) || !nzchar(matrix_col)) return(character(0))
  hits <- matrix_long[matrix_long$matrix_col == matrix_col & matrix_long$present, , drop = FALSE]
  if (!nrow(hits)) return(character(0))
  task_ids <- unique(hits$task_id)
  sort_task_ids(task_ids)
}

study_present_labels <- function(matrix_long, matrix_col, desc_lookup) {
  ids <- study_present_subtasks(matrix_long, matrix_col, desc_lookup)
  if (!length(ids)) return(character(0))
  vapply(ids, function(id) task_label(id, desc_lookup), character(1), USE.NAMES = FALSE)
}

warn_missing_matrix_cols <- function(browse_base, matrix_long) {
  known <- unique(matrix_long$matrix_col)
  missing <- unique(browse_base$matrix_col[
    !is.na(browse_base$matrix_col) &
      nzchar(browse_base$matrix_col) &
      !browse_base$matrix_col %in% known
  ])
  if (length(missing)) {
    warning(
      "No va-matrix column for: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(missing)
}

egra_subtask_copy <- function() {
  list(
    list_comp = list(
      title = "Listening Comprehension",
      body = paste(
        "Comprehension is the main goal of reading—understanding what is read.",
        "Comprehension is a complex task that requires some ability in all other reading skills.",
        "In this subtask, the assessor read a passage to the student, who did not see it.",
        "The student then responded to questions or statements read by the assessor.",
        "The listening comprehension score is the total correct answers."
      )
    ),
    letter = list(
      title = "Letter Name Knowledge",
      body = paste(
        "The letter name knowledge subtask measures a student's ability to identify",
        "letter names. Students are shown letters and asked to name them."
      )
    ),
    letter_sound = list(
      title = "Letter Sound Knowledge",
      body = paste(
        "The letter sound knowledge subtask measures a student's ability to identify",
        "the sounds associated with letters."
      )
    ),
    pa_init_sound = list(
      title = "Initial Sound",
      body = paste(
        "The initial sound subtask is a measure of a student's ability to identify the",
        "first sound in a word. It also measures a student's ability to separate words",
        "into sounds and to manipulate those sounds.",
        "Students were told a word verbally and asked to isolate and pronounce the",
        "first sound of the word (the initial sound)."
      )
    ),
    phoneme_seg_a_en = list(
      title = "Phoneme Segmentation",
      body = paste(
        "The phoneme segmentation subtask measures a student's ability to break words",
        "into individual phonemes."
      )
    ),
    syllable_sound = list(
      title = "Syllable Sounds",
      body = paste(
        "The syllable identification subtask tests students' ability to recognize syllables.",
        "The students were presented with a grid of syllables and asked to pronounce",
        "as many of the syllables' sounds as possible."
      )
    ),
    invent_word = list(
      title = "Nonwords",
      body = paste(
        "The nonword subtask tests students' skill in using letter-sound connections to",
        "figure out (\"decode\") words. While many students learn to memorize a broad range",
        "of \"sight\" words, they need skills to decode less-familiar words.",
        "In this subtask, students were given a list of made-up words that do not exist",
        "in the language tested but follow a typical spelling/sound combination of the language."
      )
    ),
    fam_word = list(
      title = "Familiar Words",
      body = paste(
        "The familiar word reading subtask is similar in format to the nonword reading",
        "subtask except that it presents the student with words they are expected to be",
        "able to read at their grade level and have likely encountered before.",
        "Students were instructed to read aloud as many words as they could in one minute."
      )
    ),
    oral_read = list(
      title = "Oral Reading Fluency (ORF)",
      body = paste(
        "The oral reading fluency (ORF) subtask measures how quickly and accurately a",
        "student can read. It is a core component of EGRA because it brings together",
        "lower-level reading skills (such as decoding and familiar word recognition)",
        "with how quickly and easily the student can read a given word (called automaticity).",
        "Students were given a short, written passage on a topic that was familiar to them.",
        "They were asked to read it out loud \"quickly but carefully.\"",
        "The score is reported as correct words per minute (cwpm)."
      )
    ),
    read_comp = list(
      title = "Reading Comprehension",
      body = paste(
        "Comprehension is the main goal of reading—understanding what is read.",
        "Comprehension is a complex task that requires some ability in all other reading skills.",
        "This subtask is paired with the ORF subtask. Depending on how much of the ORF",
        "passage the student was able to read, the assessor asked the student questions",
        "about the story. The score is the number of questions answered correctly."
      )
    ),
    maze = list(
      title = "Maze / Cloze",
      body = paste(
        "The maze or cloze subtask measures reading comprehension by asking students to",
        "select appropriate words to complete sentences in a passage."
      )
    ),
    vocab = list(
      title = "Vocabulary",
      body = paste(
        "The vocabulary subtask measures students' knowledge of word meanings."
      )
    ),
    oral_vocab = list(
      title = "Oral Vocabulary",
      body = paste(
        "The oral vocabulary subtask measures students' spoken vocabulary knowledge."
      )
    ),
    num_id = list(
      title = "Number Identification",
      body = paste(
        "The number identification subtask measures whether a student can recognise and name",
        "or select numbers. Students are presented with numbers and asked to identify them.",
        "The score is typically the number of items answered correctly."
      )
    ),
    quant_comp = list(
      title = "Number Discrimination",
      body = paste(
        "The number discrimination subtask measures a student's ability to compare quantities",
        "or identify which of two or more numbers or sets is larger, smaller, or the same.",
        "It assesses early numeracy and magnitude understanding."
      )
    ),
    addlvl1 = list(
      title = "Addition Level 1",
      body = paste(
        "Addition level 1 measures a student's ability to solve basic addition problems,",
        "usually with small numbers appropriate to the grade.",
        "The score is the number of problems answered correctly."
      )
    ),
    addlvl2 = list(
      title = "Addition Level 2",
      body = paste(
        "Addition level 2 measures a student's ability to solve more difficult addition",
        "problems than level 1, often involving larger numbers or additional steps.",
        "The score is the number of problems answered correctly."
      )
    ),
    sublvl1 = list(
      title = "Subtraction Level 1",
      body = paste(
        "Subtraction level 1 measures a student's ability to solve basic subtraction problems.",
        "The score is the number of problems answered correctly."
      )
    ),
    sublvl2 = list(
      title = "Subtraction Level 2",
      body = paste(
        "Subtraction level 2 measures a student's ability to solve more difficult subtraction",
        "problems than level 1. The score is the number of problems answered correctly."
      )
    ),
    miss_num = list(
      title = "Word Problems",
      body = paste(
        "The word problems subtask measures a student's ability to solve short mathematics",
        "story problems. Students must understand the problem, choose the correct operation,",
        "and find the answer. The score is the number of problems answered correctly."
      )
    ),
    word_prob = list(
      title = "Missing Number",
      body = paste(
        "The missing number subtask measures a student's ability to find an unknown value",
        "in a number sequence or equation (for example, 3 + \u25a1 = 7).",
        "The score is the number of items answered correctly."
      )
    ),
    pa_df_init_snd = list(
      title = "Different Initial Sound",
      body = paste(
        "This subtask measures whether a student can identify words that begin with a",
        "different sound from a reference word.",
        "The score is the number of items answered correctly."
      )
    ),
    pa_df_fnl_snd = list(
      title = "Different Final Sound",
      body = paste(
        "This subtask measures whether a student can identify words that end with a",
        "different sound from a reference word.",
        "The score is the number of items answered correctly."
      )
    ),
    pa_num_sound = list(
      title = "Phoneme Segmentation",
      body = paste(
        "This subtask measures a student's ability to break spoken words into their",
        "individual sounds (phonemes) and produce or count them.",
        "The score reflects correct segmentation or sound production."
      )
    ),
    dict_let = list(
      title = "Letter Dictation",
      body = paste(
        "The letter dictation subtask measures a student's ability to write letters from",
        "dictation. The assessor says a letter name or sound and the student writes the",
        "corresponding letter. The score is the number of letters written correctly."
      )
    ),
    word_dict = list(
      title = "Word Dictation",
      body = paste(
        "The word dictation subtask measures a student's ability to spell words from",
        "dictation. The assessor says a word and the student writes it.",
        "The score is the number of words spelled correctly."
      )
    )
  )
}

normalize_task_key <- function(x) {
  gsub("[^a-z0-9]", "", tolower(trimws(as.character(x))))
}

copy_entry <- function(task_id, copy_lookup) {
  key <- normalize_task_key(task_id)
  hits <- names(copy_lookup)[normalize_task_key(names(copy_lookup)) == key]
  if (length(hits)) return(copy_lookup[[hits[1]]])
  NULL
}

task_label <- function(task_id, desc_lookup, copy_lookup = NULL) {
  key <- normalize_task_key(task_id)
  if (!is.null(copy_lookup)) {
    entry <- copy_entry(task_id, copy_lookup)
    if (!is.null(entry)) return(entry$title)
  }
  hit <- desc_lookup$task_label[!is.na(desc_lookup$task_id) & desc_lookup$task_id == key]
  if (length(hit)) return(hit[1])
  stringr::str_to_title(gsub("_", " ", key))
}

task_description <- function(task_id, copy_lookup) {
  entry <- copy_entry(task_id, copy_lookup)
  if (!is.null(entry)) return(entry$body)
  ""
}

sort_task_ids <- function(task_ids) {
  priority <- c(
    "list_comp", "letter", "letter_sound", "pa_init_sound", "pa_df_init_snd", "pa_df_fnl_snd",
    "pa_num_sound", "phoneme_seg_a_en", "syllable_sound",
    "invent_word", "fam_word", "oral_read", "read_comp", "maze",
    "num_id", "quant_comp", "addlvl1", "addlvl2", "sublvl1", "sublvl2", "miss_num", "word_prob",
    "dict_let", "word_dict", "vocab", "oral_vocab"
  )
  keys <- normalize_task_key(task_ids)
  ord <- match(keys, priority)
  ord[is.na(ord)] <- 1000 + seq_along(ord[is.na(ord)])
  task_ids[order(ord, keys)]
}

build_task_label_lookup <- function(task_meaning) {
  cols <- names(task_meaning)
  prefix_col <- cols[grep("prefix", cols, ignore.case = TRUE)][1]
  task_col   <- cols[grep("^task", cols, ignore.case = TRUE)][1]
  alt_col    <- cols[grep("alternate", cols, ignore.case = TRUE)][1]

  rows <- lapply(seq_len(nrow(task_meaning)), function(i) {
    prefix <- normalize_task_key(task_meaning[[prefix_col]][i])
    task   <- trimws(as.character(task_meaning[[task_col]][i]))
  if (is.na(task) || task == "") return(NULL)
    out <- data.frame(key = prefix, label = task, stringsAsFactors = FALSE)
    if (!is.na(alt_col)) {
      alts <- strsplit(as.character(task_meaning[[alt_col]][i]), ",")[[1]]
      alts <- normalize_task_key(trimws(alts))
      alts <- alts[nzchar(alts)]
      if (length(alts)) {
        out <- rbind(out, data.frame(key = alts, label = task, stringsAsFactors = FALSE))
      }
    }
    out
  })
  lookup <- do.call(rbind, rows)
  lookup[!duplicated(lookup$key), , drop = FALSE]
}

get_study_masters <- function(source_id, sub_tasks) {
  if (!source_id %in% names(sub_tasks)) return(character(0))
  vals <- sub_tasks[[source_id]]
  present <- !is.na(vals) & trimws(as.character(vals)) != ""
  sub_tasks$master[present] |>
    unique() |>
    sort()
}

master_label <- function(master, label_lookup, copy_lookup) {
  key <- normalize_task_key(master)
  if (!is.null(copy_lookup[[master]])) return(copy_lookup[[master]]$title)
  if (!is.null(copy_lookup[[key]])) return(copy_lookup[[key]]$title)
  hit <- label_lookup$label[label_lookup$key == key]
  if (length(hit)) return(hit[1])
  hit2 <- label_lookup$label[label_lookup$key == normalize_task_key(master)]
  if (length(hit2)) return(hit2[1])
  stringr::str_to_title(gsub("_", " ", master))
}

master_description <- function(master, copy_lookup) {
  if (!is.null(copy_lookup[[master]])) return(copy_lookup[[master]]$body)
  key <- normalize_task_key(master)
  if (!is.null(copy_lookup[[key]])) return(copy_lookup[[key]]$body)
  ""
}

sort_masters <- function(masters) {
  sort_task_ids(masters)
}

study_detail_styles <- function() {
  HTML("
<style>
:root { --af-font: 'Source Sans 3', sans-serif; }
*, *::before, *::after { box-sizing: border-box; }
html, body,
input, button, select, textarea,
button.af-study-tab {
  font-family: var(--af-font);
}
body {
  font-size: 16px;
  background: #FFFFFF;
  color: #1F2937;
  margin: 0; padding: 0;
}
.af-logobar {
  display: flex; align-items: center; padding: 0.85rem 2rem; background: #080056;
}
.af-logo-aflearn { height: 96px; width: auto; max-width: 320px; object-fit: contain; }
.af-study-hero {
  background: #100A78; color: #fff; padding: 1.75rem 2rem 1.5rem;
  border-bottom: 3px solid #080056;
}
.af-study-hero h1 {
  margin: 0 0 0.5rem; font-size: 1.75rem; font-weight: 800; line-height: 1.25;
}
.af-back-link {
  color: #CBD5E1; text-decoration: none; font-size: 0.9rem; font-weight: 600;
}
.af-back-link:hover { color: #C8892A; }
.af-study-layout {
  display: grid; grid-template-columns: minmax(0, 1fr) 280px;
  gap: 2rem; max-width: 1200px; margin: 0 auto; padding: 1.75rem 2rem 2.5rem;
}
.af-study-tabs {
  display: flex; gap: 2.5rem; border-bottom: 1px solid #E5E7EB;
  margin-bottom: 1.75rem; padding-bottom: 0;
}
.af-study-tab {
  background: none; border: none; border-bottom: 3px solid transparent;
  padding: 0 0 0.85rem; margin: 0 0 -1px;
  font-family: var(--af-font); font-size: 1.05rem; font-weight: 800;
  color: #0F1F38; cursor: pointer; transition: color 0.15s, border-color 0.15s;
}
.af-study-tab:hover { color: #C8892A; }
.af-study-tab.active { color: #C8892A; border-bottom-color: #C8892A; }
.af-study-panel { display: none; }
.af-study-panel.active { display: block; }
.af-study-panel p {
  font-size: 1rem; line-height: 1.7; color: #374151; margin: 0 0 1rem;
}
.af-section-title {
  font-size: 1.15rem; font-weight: 800; color: #06003E; margin: 1.5rem 0 0.75rem;
}
.af-section-title:first-child { margin-top: 0; }
.af-meta-card {
  background: #F8FAFC; border: 1px solid #E5E7EB; border-radius: 8px;
  padding: 1.1rem 1.15rem;
}
.af-meta-row {
  display: flex; justify-content: space-between; gap: 1rem;
  padding: 0.55rem 0; border-bottom: 1px solid #E5E7EB; font-size: 0.92rem;
}
.af-meta-row:last-child { border-bottom: none; }
.af-meta-label { color: #64748B; font-weight: 700; text-transform: uppercase; font-size: 0.72rem; letter-spacing: 0.06em; }
.af-meta-value { color: #06003E; font-weight: 700; text-align: right; }
.af-subtask-list { display: flex; flex-direction: column; gap: 0.65rem; margin-top: 0.5rem; }
.af-subtask-item {
  border: 1px solid #E5E7EB; border-radius: 8px; background: #FFFFFF;
}
.af-subtask-item summary {
  cursor: pointer; padding: 0.9rem 1rem; font-weight: 800; color: #06003E;
  list-style: none; display: flex; justify-content: space-between; align-items: center;
}
.af-subtask-item summary::-webkit-details-marker { display: none; }
.af-subtask-item summary::after { content: '+'; color: #06003E; font-size: 1.2rem; font-weight: 700; }
.af-subtask-item[open] summary::after { content: '\u00d7'; }
.af-subtask-item summary:hover { color: #C8892A; }
.af-subtask-body {
  padding: 0 1rem 1rem; font-size: 0.95rem; line-height: 1.65; color: #374151;
}
.af-svyset {
  font-family: ui-monospace, monospace; font-size: 12px; color: #0F1F38;
  background: #EFF6FF; border: 1px solid #BFDBFE; border-radius: 6px;
  padding: 0.75rem 0.85rem; white-space: pre-wrap; word-break: break-word;
  line-height: 1.55; margin: 0.75rem 0 1rem;
}
.af-footer {
  padding: 0.9rem 2rem 1.5rem; font-size: 0.82rem; color: #9CA3AF;
  border-top: 1px solid #E5E7EB; background: #FFFFFF;
}
@media (max-width: 900px) {
  .af-study-layout { grid-template-columns: 1fr; padding: 1.25rem 1rem 2rem; }
  .af-logo-aflearn { height: 72px; }
}
</style>")
}

build_study_detail_page <- function(row, present_task_ids, desc_lookup, copy_lookup,
                                    index_url, logo_uri,
                                    file_cell, link_pill, study_type_label) {
  subtask_items <- lapply(present_task_ids, function(task_id) {
    title <- task_label(task_id, desc_lookup, copy_lookup)
    body  <- task_description(task_id, copy_lookup)
    tags$details(
      class = "af-subtask-item",
      tags$summary(title),
      tags$div(
        class = "af-subtask-body",
        if (nzchar(body)) tags$p(body) else tags$p(style = "color:#94A3B8; font-style:italic;", "Description coming soon.")
      )
    )
  })

  subtask_summary <- if (length(present_task_ids)) {
    paste(vapply(present_task_ids, function(id) task_label(id, desc_lookup, copy_lookup),
                 character(1)), collapse = ", ")
  } else {
    "\u2014"
  }

  has_egma <- any(desc_lookup$task_id %in% present_task_ids &
                    grepl("EGMA", desc_lookup$assessment, ignore.case = TRUE))
  has_egra <- any(desc_lookup$task_id %in% present_task_ids &
                    grepl("EGRA", desc_lookup$assessment, ignore.case = TRUE))
  assessment_label <- if (has_egma && has_egra) {
    "EGRA / EGMA"
  } else if (has_egma) {
    "EGMA"
  } else {
    "EGRA"
  }

  desc <- ifelse(is.na(row$Description) || row$Description == "", "—", row$Description)
  sampling <- ifelse(is.na(row$Sampling) || row$Sampling == "", "—", row$Sampling)
  svyset <- ifelse(is.na(row$Svyset) || row$Svyset == "", "— not specified —", row$Svyset)

  title_text <- glue("{row$Year} {row$Study_Name}")

  tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title(title_text),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,400;0,500;0,600;0,700;0,800;1,400&display=swap",
        rel = "stylesheet"
      ),
      study_detail_styles()
    ),
    tags$body(
      tags$div(
        class = "af-logobar",
        tags$a(href = index_url, tags$img(src = logo_uri, class = "af-logo-aflearn", alt = "AFLearn DataHub"))
      ),
      tags$div(
        class = "af-study-hero",
        tags$a(class = "af-back-link", href = index_url, "\u2190 All study rounds"),
        tags$h1(title_text)
      ),
      tags$div(
        class = "af-study-layout",
        tags$div(
          class = "af-study-main",
          tags$nav(
            class = "af-study-tabs",
            `aria-label` = "Study sections",
            tags$button(type = "button", class = "af-study-tab active", `data-panel` = "about", "About the Program"),
            tags$button(type = "button", class = "af-study-tab", `data-panel` = "sampling", "Sampling Description")
          ),
          tags$div(id = "panel-about", class = "af-study-panel active",
            tags$p(desc),
            tags$h2(class = "af-section-title", "Assessment Subtasks"),
            tags$p(style = "color:#64748B; font-size:0.95rem; margin-bottom:1rem;",
              "The subtasks included in this study round are described below."),
            if (length(subtask_items)) {
              tags$div(class = "af-subtask-list", subtask_items)
            } else {
              tags$p(style = "color:#94A3B8; font-style:italic;",
                "Sub-task availability for this study round is not yet listed in the variable availability matrix.")
            }
          ),
          tags$div(id = "panel-sampling", class = "af-study-panel",
            tags$h2(class = "af-section-title", "Sampling"),
            tags$p(sampling),
            tags$h2(class = "af-section-title", "svyset command"),
            if (svyset == "— not specified —") {
              tags$p(style = "color:#94A3B8; font-style:italic;", svyset)
            } else {
              tags$pre(class = "af-svyset", svyset)
            },
            tags$h2(class = "af-section-title", "Data files"),
            tags$div(file_cell(row$DataFile_text, row$DataFile_url)),
            tags$h2(class = "af-section-title", "Codebook"),
            tags$div(file_cell(row$Codebook_text, row$Codebook_url))
          )
        ),
        tags$aside(
          class = "af-study-meta",
          tags$div(
            class = "af-meta-card",
            tags$div(class = "af-meta-row",
              tags$span(class = "af-meta-label", "Year"),
              tags$span(class = "af-meta-value", row$Year)
            ),
            tags$div(class = "af-meta-row",
              tags$span(class = "af-meta-label", "Grade(s)"),
              tags$span(class = "af-meta-value", row$Grades)
            ),
            tags$div(class = "af-meta-row",
              tags$span(class = "af-meta-label", "Language"),
              tags$span(class = "af-meta-value", row$Languages)
            ),
            tags$div(class = "af-meta-row",
              tags$span(class = "af-meta-label", "Sub-tasks"),
              tags$span(class = "af-meta-value", subtask_summary)
            ),
            tags$div(class = "af-meta-row",
              tags$span(class = "af-meta-label", "Country"),
              tags$span(class = "af-meta-value", row$Country)
            ),
            tags$div(class = "af-meta-row",
              tags$span(class = "af-meta-label", "Assessment"),
              tags$span(class = "af-meta-value", assessment_label)
            ),
            tags$div(class = "af-meta-row",
              tags$span(class = "af-meta-label", "Study type"),
              tags$span(class = "af-meta-value", study_type_label(row$Study_Type))
            ),
            if (!is.na(row$DataLumos) && row$DataLumos != "") {
              tags$div(
                style = "margin-top:0.85rem;",
                link_pill(row$DataLumos, "Project on DataLumos \u2197", "#0F1F38", "#F0F4FF", "#C7D7F5")
              )
            }
          )
        )
      ),
      tags$div(
        class = "af-footer",
        tags$strong("AFLearn Harmonised EGRA/EGMA Dataset"),
        " \u00b7 DataFirst, University of Cape Town"
      ),
      tags$script(HTML("
document.querySelectorAll('.af-study-tab').forEach(function(btn) {
  btn.addEventListener('click', function() {
    var panel = btn.getAttribute('data-panel');
    document.querySelectorAll('.af-study-tab').forEach(function(b) { b.classList.remove('active'); });
    document.querySelectorAll('.af-study-panel').forEach(function(p) { p.classList.remove('active'); });
    btn.classList.add('active');
    document.getElementById('panel-' + panel).classList.add('active');
  });
});
"))
    )
  )
}

write_study_detail_pages <- function(browse_base, subtask_desc, va_matrix,
                                     studies_dir, logo_uri,
                                     file_cell, link_pill, study_type_label) {
  copy_lookup <- egra_subtask_copy()
  dir.create(studies_dir, showWarnings = FALSE, recursive = TRUE)
  old_files <- list.files(studies_dir, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)
  if (length(old_files)) unlink(old_files)

  warn_missing_matrix_cols(browse_base, va_matrix)

  for (i in seq_len(nrow(browse_base))) {
    slug <- browse_base$study_slug[i]
    row  <- browse_base[i, ]
    present_task_ids <- study_present_subtasks(va_matrix, row$matrix_col, subtask_desc)
    page <- build_study_detail_page(
      row, present_task_ids, subtask_desc, copy_lookup,
      study_index_url(slug), logo_uri,
      file_cell, link_pill, study_type_label
    )
    outfile <- file.path(studies_dir, paste0(slug, ".html"))
    dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
    htmltools::save_html(page, file = outfile)
  }
  invisible(nrow(browse_base))
}
