-- Create syntax for TABLE 'source'
CREATE TABLE `source` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL DEFAULT '',
  `media_id` varchar(255) NOT NULL DEFAULT '',
  `video_id` int(11) unsigned NOT NULL,
  `created` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`,`media_id`),
  KEY `source_video_id-video_id` (`video_id`),
  CONSTRAINT `source_video_id-video_id` FOREIGN KEY (`video_id`) REFERENCES `video` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Create syntax for TABLE 'source_field'
CREATE TABLE `source_field` (
  `source_id` int(11) unsigned NOT NULL,
  `name` varchar(255) NOT NULL DEFAULT '',
  `value` varchar(8000) DEFAULT NULL,
  `created` timestamp NOT NULL DEFAULT current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`source_id`,`name`),
  CONSTRAINT `source_field_source_id-source_id` FOREIGN KEY (`source_id`) REFERENCES `source` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Create syntax for TABLE 'tag'
CREATE TABLE `tag` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `video_id` int(11) unsigned NOT NULL,
  `name` varchar(255) NOT NULL DEFAULT '',
  PRIMARY KEY (`id`),
  UNIQUE KEY `video_id` (`video_id`,`name`),
  CONSTRAINT `tag_video_id-video_id` FOREIGN KEY (`id`) REFERENCES `video` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Create syntax for TABLE 'video'
CREATE TABLE `video` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `title` varchar(1500) NOT NULL DEFAULT '',
  `slug` varchar(1500) NOT NULL DEFAULT '',
  `publish` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `expires` timestamp NULL DEFAULT NULL,
  `file_uri` varchar(3000) NOT NULL DEFAULT '',
  `md5` varchar(32) NOT NULL DEFAULT '',
  `width` smallint(5) unsigned NOT NULL,
  `height` smallint(5) unsigned NOT NULL,
  `duration` int(10) unsigned NOT NULL,
  `thumbnail_uri` varchar(3000) DEFAULT NULL,
  `description` text NOT NULL,
  `cms_id` varchar(255) DEFAULT NULL,
  `link` varchar(3000) DEFAULT NULL,
  `canonical_source_id` int(11) unsigned NOT NULL,
  `created` timestamp NOT NULL DEFAULT current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`id`),
  KEY `video_canonical_source_id-source_id` (`canonical_source_id`),
  CONSTRAINT `video_canonical_source_id-source_id` FOREIGN KEY (`canonical_source_id`) REFERENCES `source` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Create syntax for TABLE 'video_field'
CREATE TABLE `video_field` (
  `video_id` int(11) unsigned NOT NULL,
  `name` varchar(255) NOT NULL DEFAULT '',
  `value` varchar(8000) DEFAULT NULL,
  `created` timestamp NOT NULL DEFAULT current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`video_id`,`name`),
  CONSTRAINT `video_field_video_id-video_id` FOREIGN KEY (`video_id`) REFERENCES `video` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;