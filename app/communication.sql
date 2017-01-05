/*
 Navicat Premium Data Transfer

 Source Server         : ize
 Source Server Type    : SQLite
 Source Server Version : 3008008
 Source Database       : main

 Target Server Type    : SQLite
 Target Server Version : 3008008
 File Encoding         : utf-8

 Date: 01/05/2017 11:23:49 AM
*/

PRAGMA foreign_keys = false;

-- ----------------------------
--  Table structure for input
-- ----------------------------
DROP TABLE IF EXISTS "input";
CREATE TABLE "input" (
  "unit_id" TEXT,
  "entity_id" TEXT,
  "locked" INTEGER
);

-- ----------------------------
--  Table structure for instructions
-- ----------------------------
DROP TABLE IF EXISTS "instructions";
CREATE TABLE "instructions" (
	 "instruction" TEXT
);

-- ----------------------------
--  Table structure for solution
-- ----------------------------
DROP TABLE IF EXISTS "solution";
CREATE TABLE "solution" (
  "unit_id" TEXT,
  "entity_id" TEXT,
  "locked" INTEGER
);

-- ----------------------------
--  Table structure for solution_meta
-- ----------------------------
DROP TABLE IF EXISTS "solution_meta";
CREATE TABLE "solution_meta" (
	 "timestamp" integer,
	 "score" real
);

PRAGMA foreign_keys = true;
