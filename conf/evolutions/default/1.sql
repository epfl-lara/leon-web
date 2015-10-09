# Permalink schema

# --- !Ups

CREATE TABLE permalinks (
  link VARCHAR(40),
  code VARCHAR,
  PRIMARY KEY (link)
);

# --- !Downs

DROP TABLE permalinks;

