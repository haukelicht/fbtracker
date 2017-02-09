create schema if not exists test;

create table test.posts (like posts.posts);
create table test.post_data (like posts.post_data);
create table test.post_likes (like posts.post_likes);
create table test.post_likes_rmvd (like posts.post_likes_rmvd);
create table test.post_comments (like posts.post_comments);
create table test.post_comments_rmvd (like posts.post_comments_rmvd);