# Identity schema

# --- !Ups

CREATE TABLE IF NOT EXISTS identities (
  user_id VARCHAR(40) NOT NULL,
  provider_id VARCHAR(40) NOT NULL,
  service_user_id VARCHAR(40) NOT NULL,

  first_name VARCHAR,
  last_name VARCHAR,
  full_name VARCHAR,
  email VARCHAR,
  avatar_url VARCHAR,
  auth_method VARCHAR NOT NULL,
  access_token VARCHAR,

  PRIMARY KEY (user_id, provider_id, service_user_id)
);

DROP TABLE users;

CREATE TABLE users (
  user_id VARCHAR(40) NOT NULL,
  main_provider_id VARCHAR(40) NOT NULL,

  PRIMARY KEY (user_id)
);

# --- !Downs

DROP TABLE identities;
DROP TABLE users;

CREATE TABLE IF NOT EXISTS users (
  provider_id VARCHAR(40) NOT NULL,
  user_id VARCHAR(40) NOT NULL,
  first_name VARCHAR,
  last_name VARCHAR,
  full_name VARCHAR,
  email VARCHAR,
  avatar_url VARCHAR,
  auth_method VARCHAR NOT NULL,
  access_token VARCHAR,

  PRIMARY KEY (provider_id, user_id)
);

