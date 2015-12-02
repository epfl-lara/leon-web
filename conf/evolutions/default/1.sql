# Permalink schema

# --- !Ups

CREATE TABLE IF NOT EXISTS permalinks (
  link VARCHAR(40),
  code VARCHAR,
  PRIMARY KEY (link)
);

# --- !Downs

DROP TABLE permalinks;

