# CookieAuthenticator schema

# --- !Ups

CREATE TABLE IF NOT EXISTS cookie_authenticators (
  id VARCHAR(300),
  user_id VARCHAR(40),
  expiration_date TIMESTAMP,
  last_used TIMESTAMP,
  creation_date TIMESTAMP,

  PRIMARY KEY (id)
);

# --- !Downs

DROP TABLE cookie_authenticators;

