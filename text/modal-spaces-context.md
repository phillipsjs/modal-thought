# modal-spaces context for Modal thought

The first example dataset for `Modal thought` comes from the `modal-spaces` project. This repo contains data and materials for experiments on modal cognition, including a multi-study Qualtrics dataset and scenario-based coding.

Key elements:

- `external/modal-spaces/data/`
  - Contains raw survey exports for Study 1a, Study 1b, Study 1c, Study 2, Study 3, Study 4, Study 5a, Study 5b, Study 5c, and Study 6.
  - These are the starting datasets for interactive exploration.

- `external/modal-spaces/materials/contextsTable.csv`
  - Defines the scenarios and response actions used in the study.
  - Useful for mapping the survey fields to the experimental context.

- `external/modal-spaces/manualCoding/textsCodingKey.csv`
  - Provides coding categories for text responses.
  - Helps us interpret and annotate free-response data.

Why this dataset is a good first example:

- It's already structured around modal scenarios and responses.
- It connects directly to the research question of how people reason about possibilities and actions.
- It supports both versioned narrative and interactive data exploration.

Next step:

- Build a simple explorer page that loads `study1a.csv` and displays scenario context plus response counts.
- Use `contextsTable.csv` to label scenarios, and `textsCodingKey.csv` to interpret text coding where needed.
