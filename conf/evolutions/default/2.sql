# User schema

# --- !Ups

CREATE TABLE IF NOT EXISTS users (
  provider_id VARCHAR(40),
  user_id VARCHAR(40),
  first_name VARCHAR,
  last_name VARCHAR,
  full_name VARCHAR,
  email VARCHAR,
  avatar_url VARCHAR,
  auth_method VARCHAR NOT NULL,
  access_token VARCHAR,

  PRIMARY KEY (provider_id, user_id)
);

# --- !Downs

DROP TABLE users;
