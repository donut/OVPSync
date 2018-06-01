-- Create syntax for TABLE 'source'
CREATE TABLE `source` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL DEFAULT '',
  `media_id` varchar(255) NOT NULL DEFAULT '',
  `video_id` int(11) unsigned DEFAULT NULL,
  `added` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `modified` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `created` timestamp NOT NULL DEFAULT current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`,`media_id`),
  KEY `source.video_id->video.id` (`video_id`),
  CONSTRAINT `source.video_id->video.id` FOREIGN KEY (`video_id`) REFERENCES `video` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Create syntax for TABLE 'source_field'
CREATE TABLE `source_field` (
  `source_id` int(11) unsigned NOT NULL,
  `name` varchar(255) NOT NULL DEFAULT '',
  `value` varchar(8000) NOT NULL DEFAULT '',
  `created` timestamp NOT NULL DEFAULT current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`source_id`,`name`),
  CONSTRAINT `source_field.source_id->source.id` FOREIGN KEY (`source_id`) REFERENCES `source` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Create syntax for TABLE 'tag'
CREATE TABLE `tag` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL DEFAULT '',
  `created` timestamp NOT NULL DEFAULT current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Create syntax for TABLE 'variable'
CREATE TABLE `variable` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `namespace` varchar(255) NOT NULL DEFAULT '',
  `key` varchar(255) NOT NULL DEFAULT '',
  `value` longtext NOT NULL,
  `created` timestamp NOT NULL DEFAULT current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`id`),
  UNIQUE KEY `namespace` (`namespace`,`key`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Create syntax for TABLE 'video'
CREATE TABLE `video` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `title` varchar(1500) NOT NULL DEFAULT '',
  `slug` varchar(1500) NOT NULL DEFAULT '',
  `publish` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `expires` timestamp NULL DEFAULT NULL,
  `file_uri` varchar(3000) DEFAULT NULL,
  `md5` varchar(32) DEFAULT NULL,
  `width` smallint(5) unsigned DEFAULT NULL,
  `height` smallint(5) unsigned DEFAULT NULL,
  `duration` int(10) unsigned DEFAULT NULL,
  `thumbnail_uri` varchar(3000) DEFAULT NULL,
  `description` text DEFAULT NULL,
  `cms_id` varchar(255) DEFAULT NULL,
  `link` varchar(3000) DEFAULT NULL,
  `canonical_source_id` int(11) unsigned NOT NULL,
  `created` timestamp NOT NULL DEFAULT current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`id`),
  KEY `video.canonical_source_id->source.id` (`canonical_source_id`),
  CONSTRAINT `video.canonical_source_id->source.id` FOREIGN KEY (`canonical_source_id`) REFERENCES `source` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Create syntax for TABLE 'video_field'
CREATE TABLE `video_field` (
  `video_id` int(11) unsigned NOT NULL,
  `name` varchar(255) NOT NULL DEFAULT '',
  `value` varchar(8000) NOT NULL DEFAULT '',
  `created` timestamp NOT NULL DEFAULT current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`video_id`,`name`),
  CONSTRAINT `video_field.video_id->video.id` FOREIGN KEY (`video_id`) REFERENCES `video` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Create syntax for TABLE 'video_tag'
CREATE TABLE `video_tag` (
  `video_id` int(11) unsigned NOT NULL,
  `tag_id` int(11) unsigned NOT NULL,
  `created` timestamp NOT NULL DEFAULT current_timestamp(),
  `updated` timestamp NOT NULL DEFAULT current_timestamp() ON UPDATE current_timestamp(),
  PRIMARY KEY (`video_id`,`tag_id`),
  KEY `video_tag.tag_id->tag.id` (`tag_id`),
  CONSTRAINT `video_tag.tag_id->tag.id` FOREIGN KEY (`tag_id`) REFERENCES `tag` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `video_tag.video_id->video.id` FOREIGN KEY (`video_id`) REFERENCES `video` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;