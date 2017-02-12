create schema if not exists test;

create table test.posts (like posts.posts including all);
create table test.post_data (like posts.post_data including all);
create table test.post_likes (like posts.post_likes including all);
create table test.post_likes_rmvd (like posts.post_likes_rmvd including all);
create table test.post_comments (like posts.post_comments including all);
create table test.post_comments_rmvd (like posts.post_comments_rmvd including all);

