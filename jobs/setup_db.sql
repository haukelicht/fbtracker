
-- Set up data model 'fbtracker'

-- USERS
CREATE SCHEMA IF NOT EXISTS users;

-- DROP TABLE IF EXISTS users.pages CASCADE;
CREATE TABLE IF NOT EXISTS users.pages (
  page_id  VARCHAR(99) PRIMARY KEY,
  page_name  VARCHAR(99),
  load_timestamp  TIMESTAMP
);



-- DROP TABLE IF EXISTS users.page_data;
CREATE TABLE IF NOT EXISTS users.page_data (
  page_id  VARCHAR(99)
    REFERENCES users.pages(page_id),
  load_timestamp  TIMESTAMP,
  is_public  BOOLEAN,
  category  VARCHAR(99),
  likes  NUMERIC,
  PRIMARY KEY (page_id,load_timestamp)
);

-- POSTS
CREATE SCHEMA IF NOT EXISTS posts;

Drop TYPE post_type CASCADE;
CREATE TYPE post_type AS 
  ENUM ('link', 'status', 'photo', 'video', 'offer', 'event', 'note');

-- DROP TABLE IF EXISTS posts.posts CASCADE;
CREATE TABLE IF NOT EXISTS posts.posts (
  post_id  VARCHAR(99)  PRIMARY KEY,
  from_id  VARCHAR(99),
  created_time TIMESTAMP,
  message TEXT, 
  post_type VARCHAR(15), 
  post_link VARCHAR(300),
  load_timestamp  TIMESTAMP
);

-- DROP TABLE IF EXISTS posts.post_data;
CREATE TABLE IF NOT EXISTS posts.post_data (
  post_id  VARCHAR(99)
    REFERENCES posts.posts(post_id),
  load_timestamp  TIMESTAMP,
  likes_count  NUMERIC,
  comments_count  NUMERIC,
  shares_count  NUMERIC,
  react_like_counts  NUMERIC,
  react_love_counts  NUMERIC,
  react_wow_counts  NUMERIC, 
  react_haha_counts  NUMERIC,
  react_sad_counts  NUMERIC,
  react_angry_counts  NUMERIC,
  react_thankful_counts  NUMERIC,
  react_total_counts  NUMERIC,
  PRIMARY KEY (post_id,load_timestamp)
);

-- Post Likes
-- DROP TABLE IF EXISTS posts.post_likes;
CREATE TABLE IF NOT EXISTS posts.post_likes (
  post_id  VARCHAR(99),
  user_id  VARCHAR(99),
  user_name  VARCHAR(99),
  load_timestamp  TIMESTAMP,
  PRIMARY KEY (post_id,user_id,load_timestamp)
);

-- Post Likes that were removed
-- DROP TABLE IF EXISTS posts.post_likes_rmvd;
CREATE TABLE IF NOT EXISTS posts.post_likes_rmvd (
  post_id  VARCHAR(99),
  user_id  VARCHAR(99),
  load_timestamp  TIMESTAMP,
  PRIMARY KEY (post_id,user_id,load_timestamp)
);

-- Post Comments
-- DROP TABLE IF EXISTS posts.post_comments CASCADE;
CREATE TABLE IF NOT EXISTS posts.post_comments (
  post_id  VARCHAR(99),
  cmnt_id  VARCHAR(99),
  from_id  VARCHAR(99),
  from_name  VARCHAR(99),
  created_time  TIMESTAMP,
  like_count  NUMERIC,
  load_timestamp  TIMESTAMP,
  PRIMARY KEY (post_id,cmnt_id)
);

-- Post comments that were removed
-- DROP TABLE IF EXISTS posts.post_comments_rmvd;
CREATE TABLE IF NOT EXISTS posts.post_comments_rmvd (
  post_id VARCHAR(99),
--     REFERENCES posts.post_comments(post_id,cmnt_id),
  cmnt_id  VARCHAR(99),
--     REFERENCES posts.post_comments(post_id,cmnt_id),
  load_timestamp  TIMESTAMP,
  PRIMARY KEY (cmnt_id,load_timestamp)
);

-- last recorded comment per post with created time
CREATE VIEW posts.vw_last_post_comments
AS 
SELECT 
  post_id,
  max(cmnt_id)::VARCHAR(99) last_cmnt_id, 
  max(created_time) created_time
FROM posts.post_comments
GROUP BY post_id
HAVING max(cmnt_id) NOT IN (SELECT cmnt_id FROM posts.post_comments_rmvd);
  

